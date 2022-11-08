{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Specstrom.Checker where

import qualified Control.Concurrent.Async as Async
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad.Writer.Class (MonadWriter (tell))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (bimap, first, second)
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
import Specstrom.Channel (Receive, Send, newChannel, receive, send, tryReceive)
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
    acts <- mapM (\nm -> (nm,) <$> maybe (fail $ "Action/event not found " <> T.unpack nm) pure (M.lookup nm actionEnv)) actNames
    actDeps <- mapM (\nm -> maybe (fail "Action/event dependencies not found") (pure . Analysis.depOf) (M.lookup nm analysisEnv)) actNames
    rs <- for props $ \name -> do
      val <- maybe (fail "Property not found") pure (M.lookup name evalEnv)
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      initial' <- case initial of
        Just e -> liftIO $ Just . Evaluator.Thunk <$> Evaluator.newThunk evalEnv e
        Nothing -> pure Nothing
      checkProp input output actionEnv (mconcat (dep : actDeps)) val acts (("__initial__",) <$> initial')
    pure (currentModule, evalEnv, actionEnv, analysisEnv, results <> rs)

data InterpreterState
  = AwaitingInitialEvent {stateVersion :: Natural}
  | ReadingQueue {formula :: Evaluator.Residual, stateVersion :: Natural, lastState :: State, sentAction :: SentAction}

type Interpret m a = (Monad m, MonadIO m, MonadCatch m, MonadThrow m, MonadError T.Text m, MonadWriter Trace m) => m a

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr . ("INFO " <>)

logErr :: MonadIO m => String -> m ()
logErr = liftIO . hPutStrLn stderr

readStdinTo :: (MonadFail m, MonadIO m) => Send ExecutorMessage -> m ()
readStdinTo output = do
  eof <- liftIO isEOF
  unless eof $ do
    line <- liftIO BS.getLine
    case JSON.eitherDecodeStrict line of
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

data SentAction = None | Sent (PrimAction, (Name, [Evaluator.Value])) | WaitingTimeout deriving (Show)

extractActions :: Maybe (Name, [Evaluator.Value]) -> Evaluator.Env -> Evaluator.State -> (Name, Evaluator.Value) -> IO [(PrimAction, (Name, [Evaluator.Value]))]
extractActions act actionEnv s (nm, v) = do
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
          map (bimap (applyTimeout t) (const (n, args))) <$> extractActions (Just (n, args)) actionEnv s (nm, r)
        Nothing -> error $ "action not found"
    Evaluator.Object _ mp -> case act of
      Just (n, args) ->
        case (M.lookup "id" mp, M.lookup "event" mp, M.lookup "args" mp, M.lookup "timeout" mp) of
          (Just (Evaluator.LitVal (Syntax.StringLit aId)), Just aEvent, Just (Evaluator.List ls), t) | Just b <- asBoolean aEvent -> do
            let timeout = t >>= asInteger
            ls' <- mapM (toJSONValue s) ls
            pure [(A aId b ls' timeout, (n, args))]
          _ -> pure []
      Nothing ->
        case (M.lookup "id" mp, M.lookup "event" mp, M.lookup "args" mp, M.lookup "timeout" mp) of
          (Just (Evaluator.LitVal (Syntax.StringLit aId)), Just aEvent, Just (Evaluator.List ls), t) | Just b <- asBoolean aEvent -> do
            let timeout = t >>= asInteger
            ls' <- mapM (toJSONValue s) ls
            pure [(A aId b ls' timeout, (nm, []))]
    Evaluator.List ls -> concat <$> mapM (extractActions act actionEnv s) (map (nm,) ls)
    _ -> pure []
  where
    applyTimeout Nothing = Prelude.id
    applyTimeout (Just t) = \x -> x {timeout = Just t}

checkProp :: Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> [(Name, Evaluator.Value)] -> Maybe (Name, Evaluator.Value) -> IO Result
checkProp input output actionEnv dep initialFormula actions expectedEvent = do
  send output (Start dep)
  (valid, trace) <- runWriterT (runExceptT (run $ AwaitingInitialEvent 0))
  send output End
  case valid of
    Left errMsg -> pure (ErrorResult (trace <> [TraceError errMsg]))
    Right valid' -> pure (RunResult valid' trace)
  where
    run :: InterpreterState -> Interpret m Validity
    run s@AwaitingInitialEvent {stateVersion = version} = do
      logInfo "Awaiting initial event"
      msg <- receive input
      case msg of
        Performed _state -> throwError "Was not expecting an action to be performed. Trace must begin with an initial event."
        Timeout _state -> do
          logInfo "Got a timeout while awaiting the initial event. Continuing to wait."
          run (AwaitingInitialEvent {stateVersion = succ version})
        Events events firstState -> do
          logInfo ("Got initial events: " <> show events)
          expectedPrims <- do
            let f = extractActions Nothing actionEnv (toEvaluatorState (-(fromIntegral (succ version))) Nothing firstState)
            case expectedEvent of
              Just ev -> liftIO (f ev)
              Nothing -> liftIO $ concat <$> mapM f actions
          case filter (actionMatchesAnyOf events . fst) expectedPrims of
            [] -> do
              logErr (show events <> " do not match any of the expected initial primitive events: " <> show (map fst expectedPrims))
              -- TODO: Optional timeout
              send output (AwaitEvents 1000 (succ version))
              run (AwaitingInitialEvent {stateVersion = succ version})
            as -> do
              tell [TraceAction (map fst as), TraceState firstState]
              let timeout = maximumTimeout (map fst as)
              ifResidual (fromIntegral (succ version)) (map snd as) firstState initialFormula $ \r -> do
                case timeout of Just t -> send output (AwaitEvents t (succ version)); Nothing -> pure ()
                run
                  ReadingQueue
                    { formula = r,
                      stateVersion = succ version,
                      lastState = firstState,
                      sentAction = case timeout of Nothing -> None; Just _ -> WaitingTimeout
                    }
        Stale -> do
          logErr "Was not expecting a stale when awaiting initial event."
          run s
        Error errMsg -> throwError errMsg
    run ReadingQueue {formula = r, stateVersion, lastState, sentAction} = do
      logInfo "Reading queue"
      msg <- case sentAction of None -> tryReceive input; _ -> Just <$> receive input
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
                    sentAction = case timeout primact of Nothing -> None; Just _ -> WaitingTimeout
                  }
          _ -> error "Not expecting a performed"
        Just (Events events nextState) -> do
          expectedPrims <- liftIO $ concat <$> mapM (extractActions Nothing actionEnv (toEvaluatorState (fromIntegral (succ stateVersion)) Nothing nextState)) actions
          case filter (actionMatchesAnyOf events . fst) expectedPrims of
            [] -> do
              logInfo ("None of the expected events (" <> show (map fst expectedPrims) <> ") matched the received events (" <> show events <> ")")
              tell [TraceAction [], TraceState nextState]
              -- the `happened` variable should be map snd as a list of action values..
              nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (fromIntegral (succ stateVersion)) (Just []) nextState))
              ifResidual (fromIntegral (succ stateVersion)) [] nextState nextFormula $ \r' -> do
                case Evaluator.stop r' of
                  Just v -> pure (Probably v)
                  Nothing -> do
                    send output (AwaitEvents 1000 (succ stateVersion))
                    run
                      ReadingQueue
                        { formula = r',
                          stateVersion = succ stateVersion,
                          lastState = nextState,
                          sentAction = sentAction
                        }
            matchingActions -> do
              tell [TraceAction (map fst matchingActions), TraceState nextState]
              let timeout = maximumTimeout (map fst matchingActions)
              -- the `happened` variable should be map snd as a list of action values..
              nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (fromIntegral (succ stateVersion)) (Just (map snd matchingActions)) nextState))
              ifResidual (fromIntegral (succ stateVersion)) (map snd matchingActions) nextState nextFormula $ \r' -> do
                case Evaluator.stop r' of
                  Just v -> pure (Probably v)
                  Nothing -> do
                    case timeout of Just t -> send output (AwaitEvents t (succ stateVersion)); Nothing -> pure ()
                    run
                      ReadingQueue
                        { formula = r',
                          stateVersion = succ stateVersion,
                          lastState = nextState,
                          sentAction = case timeout of Nothing -> None; Just _ -> WaitingTimeout
                        }
        Just (Timeout nextState) ->
          case sentAction of
            Sent (primAct, _) -> case timeout primAct of
              Just t -> do
                tell [TraceAction [timeoutOf t], TraceState nextState]
                nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (fromIntegral (succ stateVersion)) Nothing nextState))
                ifResidual (fromIntegral (succ stateVersion)) [] nextState nextFormula $ \r' ->
                  run
                    ReadingQueue
                      { formula = r',
                        stateVersion = succ stateVersion,
                        lastState = nextState,
                        sentAction = None
                      }
              Nothing -> error "Got unexpected timeout after having sent an action without a timeout."
            WaitingTimeout -> do
              tell [TraceAction [], TraceState nextState]
              nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (fromIntegral (succ stateVersion)) Nothing nextState))
              ifResidual (fromIntegral (succ stateVersion)) [] nextState nextFormula $ \r' ->
                run
                  ReadingQueue
                    { formula = r',
                      stateVersion = succ stateVersion,
                      lastState = nextState,
                      sentAction = None
                    }
            None -> error "Got unexpected timeout after not having sent an action."
        Just Stale -> logErr "Got stale" >> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
        Just (Error errMsg) -> throwError errMsg
        Nothing ->
          case Evaluator.stop r of
            Just v -> pure (Probably v)
            Nothing -> do
              possiblePrims <- liftIO $ concat <$> mapM (extractActions Nothing actionEnv (toEvaluatorState (fromIntegral stateVersion) Nothing lastState)) actions
              let acts = filter (not . isEvent . fst) possiblePrims
              case acts of
                [] -> throwError "Ran out of actions to do!"
                _ -> do
                  let len = length acts
                  idx <- liftIO (randomRIO (0, len - 1))
                  let (primaction, action) = acts !! idx
                  send output (RequestAction primaction stateVersion)
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
    Evaluator.Object _ o -> JSON.Object <$> traverse (toJSONValue st) (KM.fromHashMapText o)
    Evaluator.List o -> JSON.Array . Vector.fromList <$> traverse (toJSONValue st) o
    Evaluator.Trivial -> pure $ JSON.Bool True
    Evaluator.Absurd -> pure $ JSON.Bool False
    Evaluator.Null -> pure JSON.Null
    _ -> error "Cannot convert to JSON value"

toEvaluatorValue :: JSON.Value -> Evaluator.Value
toEvaluatorValue = \case
  JSON.String s -> Evaluator.LitVal (Syntax.StringLit s)
  JSON.Object o -> Evaluator.Object False (fmap toEvaluatorValue (KM.toHashMapText o))
  JSON.Array a -> Evaluator.List (Vector.toList (fmap toEvaluatorValue a))
  JSON.Number n -> case Scientific.floatingOrInteger n of
    Left n' -> Evaluator.LitVal (Syntax.FloatLit n')
    Right n' -> Evaluator.LitVal (Syntax.IntLit n')
  JSON.Bool True -> Evaluator.Trivial
  JSON.Bool False -> Evaluator.Absurd
  JSON.Null -> Evaluator.Null

ifResidual :: Int -> [(Name, [Evaluator.Value])] -> State -> Evaluator.Value -> (Evaluator.Residual -> Interpret m Validity) -> Interpret m Validity
ifResidual v as state formula f = do
  formula' <- liftIO (Evaluator.force (toEvaluatorState v (Just as) state) formula)
  case formula' of
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v' -> throwError (T.pack ("Unexpected value: " <> show v'))
