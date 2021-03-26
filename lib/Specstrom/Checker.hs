{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Specstrom.Checker (checkAllStdio, checkAll) where

import qualified Control.Concurrent.Async   as Async
import           Control.Monad              (foldM, unless)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Foldable              (Foldable (fold))
import qualified Data.HashMap.Strict        as M
import           Data.Maybe                 (isNothing)
import qualified Data.Scientific            as Scientific
import qualified Data.Text                  as Text
import           Data.Traversable           (for)
import qualified Data.Vector                as Vector
import           Numeric.Natural            (Natural)
import qualified Specstrom.Analysis         as Analysis
import           Specstrom.Channel          (Receive, Send, newChannel, receive,
                                             send, tryReceive)
import           Specstrom.Checker.Protocol
import           Specstrom.Dependency       (Dep)
import qualified Specstrom.Evaluator        as Evaluator
import           Specstrom.Syntax           (TopLevel (..))
import qualified Specstrom.Syntax           as Syntax
import           System.IO                  (hPutStrLn, isEOF, stderr)

checkAllStdio :: [TopLevel] -> IO [Result]
checkAllStdio ts = do
  (interpreterRecv, interpreterSend) <- newChannel
  (executorRecv, executorSend) <- newChannel
  Async.withAsync (readStdinTo executorSend) $ \inputDone ->
    Async.withAsync (writeStdoutFrom interpreterRecv) $ \outputDone -> do
      results <- checkAll executorRecv interpreterSend ts
      Async.wait inputDone
      Async.wait outputDone
      pure results

checkAll :: (MonadIO m, MonadFail m) => Receive ExecutorMessage -> Send InterpreterMessage -> [TopLevel] -> m [Result]
checkAll input output ts = do
  env <- foldM toEnv Evaluator.basicEnv ts
  let analysisEnv = Analysis.analyseTopLevels ts
  results <- fold <$> traverse (checkProps input output env analysisEnv) ts
  send output (Done results)
  pure results
  where
    toEnv :: MonadIO m => Evaluator.Env -> TopLevel -> m Evaluator.Env
    toEnv env = \case
      Binding b      -> liftIO (Evaluator.evaluateBind env b)
      Imported _ ts' -> foldM toEnv env ts'
      Properties {}  -> pure env

checkProps :: (MonadFail m, MonadIO m) => Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Analysis.AnalysisEnv -> TopLevel -> m [Result]
checkProps input output initialEnv analysisEnv = \case
  Properties _ _propGlob _ _ -> do
    let props = M.toList (M.filterWithKey (\k _ -> "prop" `Text.isPrefixOf` k) initialEnv)
    for props $ \(name, val) -> do
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      liftIO (putStrLn ("Checking property: " <> Text.unpack name))
      checkProp input output initialEnv dep val (Evaluator.Action (Evaluator.A Evaluator.Loaded Nothing))
  _ -> pure []

data InterpreterState
  = AwaitingInitialEvent {expectedEvent :: Evaluator.Value, formulaValue :: Evaluator.Value}
  | ReadingQueue {formula :: Evaluator.Residual, stateVersion :: Natural, sentAction :: Maybe Evaluator.Action}

type Interpret m a = MonadIO m => WriterT Trace m a

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr . ("INFO " <>)


logErr :: MonadIO m => String -> m ()
logErr = liftIO . hPutStrLn stderr . ("ERROR " <>)

readStdinTo :: MonadIO m => Send ExecutorMessage -> m ()
readStdinTo output = do
  eof <- liftIO isEOF
  unless eof $ do
    line <- liftIO getLine
    case JSON.eitherDecodeStrict (BS.pack line) of
      Left err -> logErr ("Receive message parsing failed: " <> err)
      Right msg -> do
        logInfo ("Read " <> show msg)
        send output msg
    readStdinTo output

writeStdoutFrom :: Receive InterpreterMessage -> IO ()
writeStdoutFrom output = do
  let write msg = do
        logInfo ("Writing " <> show msg)
        LBS.putStrLn (JSON.encode msg)
  receive output >>= \case
    d@Done {} -> write d
    msg       -> write msg >> writeStdoutFrom output

checkProp :: MonadIO m => Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> Evaluator.Value -> m Result
checkProp input output _env deps initialFormula expectedEv = do
  send output (Start { dependencies = deps })
  (valid, trace) <- runWriterT (run AwaitingInitialEvent {formulaValue = initialFormula, expectedEvent = expectedEv })
  pure (Result valid trace)
  where
    run :: InterpreterState -> Interpret m Validity
    run s@AwaitingInitialEvent {expectedEvent, formulaValue} = do
      logInfo "State: AwaitingInitialEvent"
      msg <- receive input
      logInfo "Got initial message"
      case msg of
        Performed _state -> error "Was not expecting an action to be performed. Trace must begin with an initial event."
        Event event firstState -> do
          expected <- liftIO $ Evaluator.force (toEvaluatorState firstState) expectedEvent
          case expected of
            Evaluator.Action (Evaluator.A base _timeout) ->
              if base == event
                then do
                  tell [TraceAction (Evaluator.A event Nothing), TraceState firstState]
                  ifResidual firstState formulaValue $ \r ->
                    run ReadingQueue {formula = r, stateVersion = 0, sentAction = Nothing}
                else do
                  -- maybe want to log this
                  expectedEvent' <- liftIO $ Evaluator.resetThunks expectedEvent
                  run (AwaitingInitialEvent {expectedEvent = expectedEvent', formulaValue})
            _ -> error "Provided initial event is not an action"
        Stale -> do
          logErr "Was not expecting a stale when awaiting initial event."
          run s
    run ReadingQueue {formula = r, stateVersion, sentAction} = do
      msg <- if isNothing sentAction then tryReceive input else Just <$> receive input
      case msg of
        Just (Performed nextState) -> case sentAction of
          Nothing -> error "Not expecting a performed"
          Just act -> do
            nextFormula <- liftIO (Evaluator.step r (toEvaluatorState nextState))
            tell [TraceAction act, TraceState nextState]
            ifResidual nextState nextFormula $ \r' ->
              run ReadingQueue {formula = r', stateVersion = succ stateVersion, sentAction = Nothing}
        Just (Event event nextState) -> do
          -- TODO: filter by available events
          -- TODO: Handle timeouts
          nextFormula <- liftIO (Evaluator.step r (toEvaluatorState nextState))
          tell [TraceAction (Evaluator.A event Nothing), TraceState nextState]
          ifResidual nextState nextFormula $ \r' ->
            run ReadingQueue {formula = r', stateVersion = succ stateVersion, sentAction = sentAction}
        Just Stale -> logErr "Got stale" >> run ReadingQueue {formula = r, stateVersion, sentAction = Nothing}
        Nothing ->
          case Evaluator.stop r of
            Just v -> pure (Probably v)
            Nothing -> do
              let action = Evaluator.A (Evaluator.Click "foo") Nothing
              -- TODO: pick action from spec based on current state
              -- TODO: handle timeouts
              send output (RequestAction action)
              run ReadingQueue {formula = r, stateVersion, sentAction = Just action}

toEvaluatorState :: State -> Evaluator.State
toEvaluatorState = fmap (fmap toEvaluatorValue)

toEvaluatorValue :: JSON.Value -> Evaluator.Value
toEvaluatorValue = \case
  JSON.String s -> Evaluator.LitVal (Syntax.StringLit s)
  JSON.Object o -> Evaluator.Object (fmap toEvaluatorValue o)
  JSON.Array a -> Evaluator.List (Vector.toList (fmap toEvaluatorValue a))
  JSON.Number n -> case Scientific.floatingOrInteger n of
    Left n'  -> Evaluator.LitVal (Syntax.FloatLit n')
    Right n' -> Evaluator.LitVal (Syntax.IntLit n')
  JSON.Bool True -> Evaluator.Trivial
  JSON.Bool False -> Evaluator.Absurd
  JSON.Null -> Evaluator.Null

ifResidual :: State -> Evaluator.Value -> (Evaluator.Residual -> Interpret m Validity) -> Interpret m Validity
ifResidual state formula f =
  liftIO (Evaluator.force (toEvaluatorState state) formula) >>= \case
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v -> do
      logErr ("Unexpected value: " <> show v)
      pure (Probably False)
