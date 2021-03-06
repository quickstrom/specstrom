{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Specstrom.Checker where

import qualified Control.Concurrent.Async as Async
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Aeson as JSON
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Numeric.Natural (Natural)
import qualified Specstrom.Analysis as Analysis
import Specstrom.Channel (Receive, Send, newChannel, receive, send, tryReceive, tryReceiveTimeout)
import Specstrom.Checker.Protocol
import Specstrom.Dependency (Dep)
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Syntax (Name, TopLevel, TopLevel' (..))
import qualified Specstrom.Syntax as Syntax
import System.IO (hPutStrLn, isEOF, stderr)
import System.Random (randomRIO)

checkAllStdio :: [TopLevel] -> IO ()
checkAllStdio ts = do
  (interpreterRecv, interpreterSend) <- newChannel
  (executorRecv, executorSend) <- newChannel
  Async.withAsync (readStdinTo executorSend) $ \_inputDone ->
    Async.withAsync (writeStdoutFrom interpreterRecv) $ \outputDone -> do
      Async.withAsync (checkAll executorRecv interpreterSend ts) $ \checkerDone -> do
        void (Async.waitBoth outputDone checkerDone)
          `catch` \BlockedIndefinitelyOnSTM {} ->
            fail "Checker failed due to premature end of input."

checkAll :: Receive ExecutorMessage -> Send InterpreterMessage -> [TopLevel] -> IO ()
checkAll input output ts = do
  (_, _, _, _, rs) <- checkTopLevels input output ([], []) Evaluator.basicEnv Evaluator.emptyEnv Analysis.builtIns [] ts
  send output (Done rs)

checkTopLevels :: Receive ExecutorMessage -> Send InterpreterMessage -> ([Syntax.Name], [Syntax.Name]) -> Evaluator.Env -> Evaluator.Env -> Analysis.AnalysisEnv -> [Result] -> [TopLevel] -> IO (([Syntax.Name], [Syntax.Name]), Evaluator.Env, Evaluator.Env, Analysis.AnalysisEnv, [Result])
checkTopLevels _ _ currentModule evalEnv actionEnv analysisEnv results [] = pure (currentModule, evalEnv, actionEnv, analysisEnv, results)
checkTopLevels input output currentModule evalEnv actionEnv analysisEnv results (tl : rest) = do
  (currentModule', evalEnv', actionEnv', analysisEnv', results') <- checkTopLevel input output currentModule evalEnv actionEnv analysisEnv results tl
  checkTopLevels input output currentModule' evalEnv' actionEnv' analysisEnv' results' rest

checkTopLevel ::
  Receive ExecutorMessage ->
  Send InterpreterMessage ->
  ([Syntax.Name], [Syntax.Name]) ->
  Evaluator.Env ->
  Evaluator.Env -> -- action bodies
  Analysis.AnalysisEnv ->
  [Result] ->
  TopLevel ->
  IO (([Syntax.Name], [Syntax.Name]), Evaluator.Env, Evaluator.Env, Analysis.AnalysisEnv, [Result])
checkTopLevel input output currentModule evalEnv actionEnv analysisEnv results = \case
  DocBlock {} -> pure (currentModule, evalEnv, actionEnv, analysisEnv, results)
  SyntaxDecl {} -> pure (currentModule, evalEnv, actionEnv, analysisEnv, results)
  MacroDecl {} -> pure (currentModule, evalEnv, actionEnv, analysisEnv, results)
  Binding _ b@(Syntax.Bind pat _) -> do
    evalEnv' <- Evaluator.evaluateBind Evaluator.dummyState evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (first (Syntax.bindPatternBoundVars pat ++) currentModule, evalEnv', actionEnv, analysisEnv', results)
  ActionDecl _ b@(Syntax.Bind pat _) -> do
    actionEnv' <- Evaluator.evaluateBind' Evaluator.dummyState actionEnv evalEnv b
    evalEnv' <- Evaluator.evaluateActionBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (second (Syntax.bindPatternBoundVars pat ++) currentModule, evalEnv', actionEnv', analysisEnv', results)
  Imported _ _ ts' -> do
    (_, e', acte', ae', results') <- checkTopLevels input output ([], []) evalEnv actionEnv analysisEnv results ts'
    pure (currentModule, e', acte', ae', results')
  Properties _ propGlob actsGlob initial -> do
    let props = Syntax.expand (fst currentModule) propGlob
    let actNames = Syntax.expand (snd currentModule) actsGlob
    acts <- mapM (\nm -> maybe (fail $ "Action/event not found " <> T.unpack nm) pure (M.lookup nm actionEnv)) actNames
    actDeps <- mapM (\nm -> maybe (fail "Action/event dependencies not found") (pure . Analysis.depOf) (M.lookup nm analysisEnv)) actNames
    rs <- for props $ \name -> do
      val <- maybe (fail "Property not found") pure (M.lookup name evalEnv)
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      initial' <- case initial of
        Just e -> liftIO $ Just . Evaluator.Thunk <$> Evaluator.newThunk evalEnv e
        Nothing -> pure Nothing
      checkProp input output actionEnv (mconcat (dep : actDeps)) val acts initial'
    pure (currentModule, evalEnv, actionEnv, analysisEnv, results <> rs)

data InterpreterState
  = AwaitingInitialEvent {stateVersion :: Natural}
  | ReadingQueue {formula :: Evaluator.Residual, stateVersion :: Natural, lastState :: State, sentAction :: SentAction}

type Interpret m a = (MonadIO m, MonadCatch m, MonadThrow m) => WriterT Trace m a

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr . ("INFO " <>)

logErr :: MonadIO m => String -> m ()
logErr = liftIO . hPutStrLn stderr

readStdinTo :: (MonadFail m, MonadIO m) => Send ExecutorMessage -> m ()
readStdinTo output = do
  eof <- liftIO isEOF
  unless eof $ do
    line <- liftIO getLine
    case JSON.eitherDecodeStrict (BS.pack line) of
      Left err -> do
        logErr ("Input message parsing failed: " <> err)
        fail "Input message parsing failed."
      Right msg -> do
        send output msg
        readStdinTo output

writeStdoutFrom :: Receive InterpreterMessage -> IO [Result]
writeStdoutFrom output = do
  let write msg = logInfo ("Writing: " <> show msg) >> LBS.putStrLn (JSON.encode msg)
  receive output >>= \case
    Done results -> write (Done results) $> results
    msg -> write msg >> writeStdoutFrom output

data SentAction = None | Sent (PrimAction, (Name, [Evaluator.Value])) | WaitingTimeout Int

extractActions :: Maybe (Name, [Evaluator.Value]) -> Evaluator.Env -> Evaluator.State -> Evaluator.Value -> IO [(PrimAction, (Name, [Evaluator.Value]))]
extractActions act actionEnv s v = do
  v' <- Evaluator.force s v
  let asBoolean Evaluator.Trivial = Just True
      asBoolean Evaluator.Absurd = Just False
      asBoolean _ = Nothing
      asInteger (Evaluator.LitVal (Syntax.IntLit i)) = Just i
      asInteger _ = Nothing
  case v' of
    Evaluator.Action n args' t ->
      case M.lookup n actionEnv of
        Just v1 -> do
          args <- mapM (Evaluator.force s) args'
          r <- Evaluator.appAll s v1 args
          map (first (applyTimeout t) . second (const $ (n, args))) <$> extractActions (Just (n, args)) actionEnv s r
        Nothing -> error $ "action not found"
    Evaluator.Object _ mp -> case act of
      Just (n, args) ->
        case (M.lookup "id" mp, M.lookup "event" mp, M.lookup "args" mp, M.lookup "timeout" mp) of
          (Just (Evaluator.LitVal (Syntax.StringLit aId)), Just aEvent, Just (Evaluator.List ls), t) | Just b <- asBoolean aEvent -> do
            let timeout = t >>= asInteger
            ls' <- mapM (toJSONValue s) ls
            pure [(A aId b ls' timeout, (n, args))]
          _ -> pure []
      Nothing -> pure []
    Evaluator.List ls -> concat <$> mapM (extractActions act actionEnv s) ls
    _ -> pure []
  where
    applyTimeout Nothing = Prelude.id
    applyTimeout (Just t) = \x -> x {timeout = Just t}

checkProp :: Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> [Evaluator.Value] -> Maybe (Evaluator.Value) -> IO Result
checkProp input output actionEnv dep initialFormula actions expectedEvent = do
  send output (Start dep)
  (valid, trace) <- runWriterT (run $ AwaitingInitialEvent 0)
  send output End
  pure (Result valid trace)
  where
    run :: MonadFail m => InterpreterState -> Interpret m Validity
    run s@AwaitingInitialEvent {stateVersion = version} = do
      logInfo "Awaiting initial event"
      msg <- receive input
      case msg of
        Performed _state -> error "Was not expecting an action to be performed. Trace must begin with an initial event."
        Event event firstState -> do
          logInfo ("Got initial event: " <> show event)
          expectedPrims <-
            let f = extractActions Nothing actionEnv (toEvaluatorState (- (fromIntegral (succ version))) Nothing firstState)
             in case expectedEvent of
                  Just ev -> liftIO $ f ev
                  Nothing -> liftIO $ concat <$> mapM f actions
          case filter (actionMatches event . fst) expectedPrims of
            [] -> do
              logErr (show event <> " does not match any of the expected primitive events: " <> show expectedPrims)
              run (AwaitingInitialEvent {stateVersion = succ version})
            as -> do
              logInfo ("Got initial event: " <> show event)
              -- the `happened` variable should be map snd as a list of action values..
              tell [TraceAction (map fst as), TraceState firstState]
              let timeout = maximumTimeout (map fst as)
              ifResidual 0 (map snd as) firstState initialFormula $ \r ->
                run
                  ReadingQueue
                    { formula = r,
                      stateVersion = 0,
                      lastState = firstState,
                      sentAction = case timeout of Nothing -> None; Just i -> WaitingTimeout i
                    }
        Stale -> do
          logErr "Was not expecting a stale when awaiting initial event."
          run s
    run ReadingQueue {formula = r, stateVersion, lastState, sentAction} = do
      logInfo "Reading queue"
      msg <- case sentAction of None -> tryReceive input; WaitingTimeout i -> tryReceiveTimeout input i; Sent _ -> Just <$> receive input
      case msg of
        Just (Performed nextState) -> case sentAction of
          Sent (primact, val) -> do
            nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (fromIntegral (succ stateVersion)) (Just [val]) nextState))
            tell [TraceAction [primact], TraceState nextState]
            ifResidual (fromIntegral (succ stateVersion)) [val] nextState nextFormula $ \r' ->
              run
                ReadingQueue
                  { formula = r',
                    stateVersion = succ stateVersion,
                    lastState = nextState,
                    sentAction = case timeout primact of Nothing -> None; Just i -> WaitingTimeout i
                  }
          _ -> error "Not expecting a performed"
        Just (Event event nextState) -> do
          expectedPrims <- liftIO $ concat <$> mapM (extractActions Nothing actionEnv (toEvaluatorState (fromIntegral (succ stateVersion)) Nothing nextState)) actions
          case filter (actionMatches event . fst) expectedPrims of
            [] -> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
            as -> do
              tell [TraceAction (map fst as), TraceState nextState]
              let timeout = maximumTimeout (map fst as)
              -- the `happened` variable should be map snd as a list of action values..
              nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (fromIntegral (succ stateVersion)) (Just (map snd as)) nextState))
              ifResidual (fromIntegral (succ stateVersion)) (map snd as) nextState nextFormula $ \r' ->
                run
                  ReadingQueue
                    { formula = r',
                      stateVersion = succ stateVersion,
                      lastState = nextState,
                      sentAction = case timeout of Nothing -> None; Just i -> WaitingTimeout i
                    }
        Just Stale -> logErr "Got stale" >> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
        Nothing ->
          case Evaluator.stop r of
            Just v -> pure (Probably v)
            Nothing -> do
              possiblePrims <- liftIO $ concat <$> mapM (extractActions Nothing actionEnv (toEvaluatorState (fromIntegral stateVersion) Nothing lastState)) actions
              let acts = filter (not . isEvent . fst) possiblePrims
              case acts of
                [] -> error "Ran out of actions to do!"
                _ -> do
                  let len = length acts
                  idx <- liftIO (randomRIO (0, len - 1))
                  let (primaction, action) = acts !! idx
                  send output (RequestAction primaction)
                  run ReadingQueue {formula = r, stateVersion, lastState = lastState, sentAction = Sent (primaction, action)}

toEvaluatorState :: Int -> Maybe [(Name, [Evaluator.Value])] -> State -> Evaluator.State
toEvaluatorState v mb s = (v, (fmap (map (\(n, vs) -> Evaluator.Action n vs Nothing)) mb, fmap toEvaluatorValue s))

toJSONValue :: Evaluator.State -> Evaluator.Value -> Evaluator.Eval JSON.Value
toJSONValue st v = do
  v' <- Evaluator.force st v
  case v' of
    Evaluator.LitVal (Syntax.StringLit s) -> pure $ JSON.String s
    Evaluator.LitVal (Syntax.IntLit i) -> pure $ JSON.Number (fromIntegral i)
    Evaluator.LitVal (Syntax.FloatLit i) -> pure $ JSON.Number (Scientific.fromFloatDigits i)
    Evaluator.Object _ o -> JSON.Object <$> traverse (toJSONValue st) o
    Evaluator.List o -> JSON.Array . Vector.fromList <$> traverse (toJSONValue st) o
    Evaluator.Trivial -> pure $ JSON.Bool True
    Evaluator.Absurd -> pure $ JSON.Bool False
    Evaluator.Null -> pure JSON.Null
    _ -> error "Cannot convert to JSON value"

toEvaluatorValue :: JSON.Value -> Evaluator.Value
toEvaluatorValue = \case
  JSON.String s -> Evaluator.LitVal (Syntax.StringLit s)
  JSON.Object o -> Evaluator.Object False (fmap toEvaluatorValue o)
  JSON.Array a -> Evaluator.List (Vector.toList (fmap toEvaluatorValue a))
  JSON.Number n -> case Scientific.floatingOrInteger n of
    Left n' -> Evaluator.LitVal (Syntax.FloatLit n')
    Right n' -> Evaluator.LitVal (Syntax.IntLit n')
  JSON.Bool True -> Evaluator.Trivial
  JSON.Bool False -> Evaluator.Absurd
  JSON.Null -> Evaluator.Null

ifResidual :: (MonadFail m) => Int -> [(Name, [Evaluator.Value])] -> State -> Evaluator.Value -> (Evaluator.Residual -> Interpret m Validity) -> Interpret m Validity
ifResidual v as state formula f = do
  formula' <- liftIO (Evaluator.force (toEvaluatorState v (Just as) state) formula)
  case formula' of
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v' -> fail ("Unexpected value: " <> show v')
