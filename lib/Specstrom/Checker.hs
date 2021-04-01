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
import Control.Exception (BlockedIndefinitelyOnSTM (..), catch)
import Control.Monad (unless, void)
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
import Specstrom.Syntax (Name, TopLevel (..))
import qualified Specstrom.Syntax as Syntax
import System.IO (hPutStrLn, isEOF, stderr)
import System.Random (randomRIO)

checkAllStdio :: [TopLevel] -> IO ()
checkAllStdio ts = do
  (interpreterRecv, interpreterSend) <- newChannel
  (executorRecv, executorSend) <- newChannel
  Async.withAsync (readStdinTo executorSend) $ \inputDone ->
    Async.withAsync (writeStdoutFrom interpreterRecv) $ \outputDone -> do
      Async.withAsync (checkAll executorRecv interpreterSend ts) $ \checkerDone -> do
        Async.wait inputDone
        -- TODO: add notion of channel being closed instead of relying on the STM exception
        void (Async.wait outputDone) `catch` \case
          BlockedIndefinitelyOnSTM {} -> fail "Checker failed due to premature end of input."
        Async.wait checkerDone `catch` \case
          BlockedIndefinitelyOnSTM {} -> fail "Checker failed due to premature end of input."

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
  Binding b@(Syntax.Bind pat _) -> do
    evalEnv' <- Evaluator.evaluateBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (first (Syntax.bindPatternBoundVars pat ++) currentModule, evalEnv', actionEnv, analysisEnv', results)
  ActionDecl b@(Syntax.Bind pat _) -> do
    actionEnv' <- Evaluator.evaluateBind' actionEnv evalEnv b
    evalEnv' <- Evaluator.evaluateActionBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (second (Syntax.bindPatternBoundVars pat ++) currentModule, evalEnv', actionEnv', analysisEnv', results)
  Imported _ ts' -> do
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

data SentAction = None | Sent (PrimAction, (Name, [Evaluator.Value])) | WaitingTimeout Int

extractActions :: Maybe (Name, [Evaluator.Value]) -> Evaluator.Env -> Evaluator.State -> Evaluator.Value -> IO [(PrimAction, (Name, [Evaluator.Value]))]
extractActions act actionEnv s v = do
  v' <- Evaluator.force s =<< Evaluator.resetThunks v
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
    Evaluator.Object mp -> case act of
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
          expectedPrims <-
            let f = extractActions Nothing actionEnv (toEvaluatorState Nothing firstState)
             in case expectedEvent of
                  Just ev -> liftIO $ f ev
                  Nothing -> liftIO $ concat <$> mapM f actions
          case filter (actionMatches event . fst) expectedPrims of
            [] -> run AwaitingInitialEvent
            as -> do
              -- the `happened` variable should be map snd as a list of action values..
              tell [TraceAction (map fst as), TraceState firstState]
              let timeout = maximumTimeout (map fst as)
              ifResidual (map snd as) firstState initialFormula $ \r ->
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
      msg <- case sentAction of None -> tryReceive input; WaitingTimeout i -> tryReceiveTimeout input i; Sent _ -> Just <$> receive input
      case msg of
        Just (Performed nextState) -> case sentAction of
          Sent (primact, val) -> do
            nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (Just [val]) nextState))
            tell [TraceAction [primact], TraceState nextState]
            ifResidual [val] nextState nextFormula $ \r' ->
              run
                ReadingQueue
                  { formula = r',
                    stateVersion = succ stateVersion,
                    lastState = nextState,
                    sentAction = case timeout primact of Nothing -> None; Just i -> WaitingTimeout i
                  }
          _ -> error "Not expecting a performed"
        Just (Event event nextState) -> do
          expectedPrims <- liftIO $ concat <$> mapM (extractActions Nothing actionEnv (toEvaluatorState Nothing nextState)) actions
          case filter (actionMatches event . fst) expectedPrims of
            [] -> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
            as -> do
              tell [TraceAction (map fst as), TraceState nextState]
              let timeout = maximumTimeout (map fst as)
              -- the `happened` variable should be map snd as a list of action values..
              nextFormula <- liftIO (Evaluator.step r (toEvaluatorState (Just (map snd as)) nextState))
              ifResidual (map snd as) nextState nextFormula $ \r' ->
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
              possiblePrims <- liftIO $ concat <$> mapM (extractActions Nothing actionEnv (toEvaluatorState Nothing lastState)) actions
              let acts = filter (not . isEvent . fst) possiblePrims
              -- actvals <- liftIO $ mapM (Evaluator.force (toEvaluatorState lastState) <=< Evaluator.resetThunks) actions
              --let acts = catMaybes $ flip map actvals $ \v -> case v of Evaluator.Action a@(Evaluator.A b _) | not (Evaluator.isEvent b) -> Just a; _ -> Nothing
              case acts of
                [] -> error "Ran out of actions to do!"
                _ -> do
                  let len = length acts
                  idx <- liftIO (randomRIO (0, len - 1))
                  let (primaction, action) = acts !! idx
                  send output (RequestAction primaction)
                  run ReadingQueue {formula = r, stateVersion, lastState = lastState, sentAction = Sent (primaction, action)}

toEvaluatorState :: Maybe [(Name, [Evaluator.Value])] -> State -> Evaluator.State
toEvaluatorState mb s = (fmap (map (\(n, vs) -> Evaluator.Action n vs Nothing)) mb, fmap toEvaluatorValue s)

toJSONValue :: Evaluator.State -> Evaluator.Value -> Evaluator.Eval JSON.Value
toJSONValue st v = do
  v' <- Evaluator.force st v
  case v' of
    Evaluator.LitVal (Syntax.StringLit s) -> pure $ JSON.String s
    Evaluator.LitVal (Syntax.IntLit i) -> pure $ JSON.Number (fromIntegral i)
    Evaluator.LitVal (Syntax.FloatLit i) -> pure $ JSON.Number (Scientific.fromFloatDigits i)
    Evaluator.Object o -> JSON.Object <$> traverse (toJSONValue st) o
    Evaluator.List o -> JSON.Array . Vector.fromList <$> traverse (toJSONValue st) o
    Evaluator.Trivial -> pure $ JSON.Bool True
    Evaluator.Absurd -> pure $ JSON.Bool False
    Evaluator.Null -> pure JSON.Null
    _ -> error "Cannot convert to JSON value"

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

ifResidual :: [(Name, [Evaluator.Value])] -> State -> Evaluator.Value -> (Evaluator.Residual -> Interpret m Validity) -> Interpret m Validity
ifResidual as state formula f = do
  formula' <- liftIO (Evaluator.force (toEvaluatorState (Just as) state) formula)
  case formula' of
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v -> do
      logErr ("Unexpected value: " <> show v)
      pure (Probably False)
