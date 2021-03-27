{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Specstrom.Checker where

import qualified Control.Concurrent.Async as Async
import Control.Monad (unless, void, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Scientific as Scientific
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Numeric.Natural (Natural)
import qualified Specstrom.Analysis as Analysis
import Specstrom.Channel (Receive, Send, newChannel, receive, send, tryReceive, tryReceiveTimeout)
import Specstrom.Checker.Protocol
import Specstrom.Dependency (Dep)
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Syntax (TopLevel (..))
import qualified Specstrom.Syntax as Syntax
import System.IO (hPutStrLn, isEOF, stderr)
import System.Random (randomRIO)

checkAllStdio :: [TopLevel] -> IO ()
checkAllStdio ts = do
  (interpreterRecv, interpreterSend) <- newChannel
  (executorRecv, executorSend) <- newChannel
  Async.withAsync (readStdinTo executorSend) $ \inputDone ->
    Async.withAsync (writeStdoutFrom interpreterRecv) $ \outputDone -> do
      checkAll executorRecv interpreterSend ts
      Async.wait inputDone
      void (Async.wait outputDone)

checkAll :: Receive ExecutorMessage -> Send InterpreterMessage -> [TopLevel] -> IO ()
checkAll input output ts = do
  (_, _, _, rs) <- checkTopLevels input output [] Evaluator.basicEnv Analysis.builtIns [] ts
  send output (Done rs)

checkTopLevels :: Receive ExecutorMessage -> Send InterpreterMessage -> [Syntax.Name] -> Evaluator.Env -> Analysis.AnalysisEnv -> [Result] -> [TopLevel] -> IO ([Syntax.Name], Evaluator.Env, Analysis.AnalysisEnv, [Result])
checkTopLevels _ _ currentModule evalEnv analysisEnv results [] = pure (currentModule, evalEnv, analysisEnv, results)
checkTopLevels input output currentModule evalEnv analysisEnv results (tl : rest) = do
  (currentModule', evalEnv', analysisEnv', results') <- checkTopLevel input output currentModule evalEnv analysisEnv results tl
  checkTopLevels input output currentModule' evalEnv' analysisEnv' results' rest

checkTopLevel :: Receive ExecutorMessage -> Send InterpreterMessage -> [Syntax.Name] -> Evaluator.Env -> Analysis.AnalysisEnv -> [Result] -> TopLevel -> IO ([Syntax.Name], Evaluator.Env, Analysis.AnalysisEnv, [Result])
checkTopLevel input output currentModule evalEnv analysisEnv results = \case
  Binding b@(Syntax.Bind pat _) -> do
    evalEnv' <- Evaluator.evaluateBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (Syntax.bindPatternBoundVars pat ++ currentModule, evalEnv', analysisEnv', results)
  Imported _ ts' -> do
    (_, e', ae', results') <- checkTopLevels input output [] evalEnv analysisEnv results ts'
    pure (currentModule, e', ae', results')
  Properties _ propGlob actsGlob initial -> do
    let props = Syntax.expand currentModule propGlob
    let actNames = Syntax.expand currentModule actsGlob
    acts <- mapM (\nm -> maybe (fail "Action/event not found") pure (M.lookup nm evalEnv)) actNames
    actDeps <- mapM (\nm -> maybe (fail "Action/event dependencies not found") (pure . Analysis.depOf) (M.lookup nm analysisEnv)) actNames
    rs <- for props $ \name -> do
      val <- maybe (fail "Property not found") pure (M.lookup name evalEnv)
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      initial' <- case initial of
        Just e -> liftIO $ Evaluator.Thunk <$> Evaluator.newThunk evalEnv e
        Nothing -> pure (Evaluator.Action (Evaluator.A Evaluator.Loaded Nothing))
      checkProp input output evalEnv (mconcat (dep : actDeps)) val acts initial'
    pure (currentModule, evalEnv, analysisEnv, results <> rs)

data InterpreterState
  = AwaitingInitialEvent
  | ReadingQueue {formula :: Evaluator.Residual, stateVersion :: Natural, lastState :: State, sentAction :: SentAction}

type Interpret m a = MonadIO m => WriterT Trace m a

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr . ("INFO " <>)

logErr :: MonadIO m => String -> m ()
logErr = liftIO . hPutStrLn stderr

readStdinTo :: MonadIO m => Send ExecutorMessage -> m ()
readStdinTo output = do
  eof <- liftIO isEOF
  unless eof $ do
    line <- liftIO getLine
    case JSON.eitherDecodeStrict (BS.pack line) of
      Left err -> logErr ("Input message parsing failed: " <> err)
      Right msg -> send output msg
    readStdinTo output

writeStdoutFrom :: Receive InterpreterMessage -> IO [Result]
writeStdoutFrom output = do
  let write msg = LBS.putStrLn (JSON.encode msg)
  receive output >>= \case
    Done results -> write (Done results) $> results
    msg -> write msg >> writeStdoutFrom output

data SentAction = None | Sent Evaluator.Action | WaitingTimeout Int

checkProp :: Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> [Evaluator.Value] -> Evaluator.Value -> IO Result
checkProp input output _env dep initialFormula actions expectedEvent = do
  send output (Start dep)
  (valid, trace) <- runWriterT (run AwaitingInitialEvent)
  send output End
  pure (Result valid trace)
  where
    run :: InterpreterState -> Interpret m Validity
    run s@AwaitingInitialEvent = do
      msg <- receive input
      case msg of
        Performed _state -> error "Was not expecting an action to be performed. Trace must begin with an initial event."
        Event event firstState -> do
          expected <- liftIO $ Evaluator.force (toEvaluatorState firstState) expectedEvent
          case expected of
            Evaluator.Action (Evaluator.A base timeout) ->
              if base == event
                then do
                  tell [TraceAction (Evaluator.A event timeout), TraceState firstState]
                  ifResidual firstState initialFormula $ \r ->
                    run
                      ReadingQueue
                        { formula = r,
                          stateVersion = 0,
                          lastState = firstState,
                          sentAction = maybe None WaitingTimeout timeout
                        }
                else do
                  -- maybe want to log this
                  _ <- liftIO $ Evaluator.resetThunks expectedEvent
                  run AwaitingInitialEvent
            _ -> error "Provided initial event is not an action"
        Stale -> do
          logErr "Was not expecting a stale when awaiting initial event."
          run s
    run ReadingQueue {formula = r, stateVersion, lastState, sentAction} = do
      msg <- case sentAction of None -> tryReceive input; WaitingTimeout i -> tryReceiveTimeout input i; Sent _ -> Just <$> receive input
      case msg of
        Just (Performed nextState) -> case sentAction of
          Sent act@(Evaluator.A _ timeout) -> do
            nextFormula <- liftIO (Evaluator.step r (toEvaluatorState nextState))
            tell [TraceAction act, TraceState nextState]
            ifResidual nextState nextFormula $ \r' ->
              run
                ReadingQueue
                  { formula = r',
                    stateVersion = succ stateVersion,
                    lastState = nextState,
                    sentAction = maybe None WaitingTimeout timeout
                  }
          _ -> error "Not expecting a performed"
        Just (Event event nextState) -> do
          actvals <- liftIO $ mapM (Evaluator.force (toEvaluatorState nextState) <=< Evaluator.resetThunks) actions
          let acts = catMaybes $ flip map actvals $ \v -> case v of Evaluator.Action a -> Just a; _ -> Nothing
          let matches = filter (\(Evaluator.A a _) -> a == event) acts
          case matches of
            [] -> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
            act@(Evaluator.A _ timeout) : _ -> do
              -- TODO: Handle timeouts
              nextFormula <- liftIO (Evaluator.step r (toEvaluatorState nextState))
              tell [TraceAction act, TraceState nextState]
              ifResidual nextState nextFormula $ \r' ->
                run
                  ReadingQueue
                    { formula = r',
                      stateVersion = succ stateVersion,
                      lastState = nextState,
                      sentAction = maybe None WaitingTimeout timeout
                    }
        Just Stale -> logErr "Got stale" >> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
        Nothing ->
          case Evaluator.stop r of
            Just v -> pure (Probably v)
            Nothing -> do
              actvals <- liftIO $ mapM (Evaluator.force (toEvaluatorState lastState) <=< Evaluator.resetThunks) actions
              let acts = catMaybes $ flip map actvals $ \v -> case v of Evaluator.Action a@(Evaluator.A b _) | not (Evaluator.isEvent b) -> Just a; _ -> Nothing
              case acts of
                [] -> error "Ran out of actions to do!"
                _ -> do
                  let len = length acts
                  idx <- liftIO (randomRIO (0, len - 1))
                  let action = acts !! idx
                  send output (RequestAction action)
                  run ReadingQueue {formula = r, stateVersion, lastState = lastState, sentAction = Sent action}

toEvaluatorState :: State -> Evaluator.State
toEvaluatorState = fmap (fmap toEvaluatorValue)

toEvaluatorValue :: JSON.Value -> Evaluator.Value
toEvaluatorValue = \case
  JSON.String s -> Evaluator.LitVal (Syntax.StringLit s)
  JSON.Object o -> Evaluator.Object (fmap toEvaluatorValue o)
  JSON.Array a -> Evaluator.List (Vector.toList (fmap toEvaluatorValue a))
  JSON.Number n -> case Scientific.floatingOrInteger n of
    Left n' -> Evaluator.LitVal (Syntax.FloatLit n')
    Right n' -> Evaluator.LitVal (Syntax.IntLit n')
  JSON.Bool True -> Evaluator.Trivial
  JSON.Bool False -> Evaluator.Absurd
  JSON.Null -> Evaluator.Null

ifResidual :: State -> Evaluator.Value -> (Evaluator.Residual -> Interpret m Validity) -> Interpret m Validity
ifResidual state formula f = do
  formula' <- liftIO (Evaluator.force (toEvaluatorState state) formula)
  case formula' of
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v -> do
      logErr ("Unexpected value: " <> show v)
      pure (Probably False)
