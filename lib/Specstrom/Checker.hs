{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Specstrom.Checker (checkAllStdio, checkAll) where

import qualified Control.Concurrent.Async as Async
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Foldable (Foldable (fold, foldl'))
import qualified Data.HashMap.Strict as M
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Numeric.Natural (Natural)
import qualified Specstrom.Analysis as Analysis
import Specstrom.Channel (Receive, Send, newChannel, receive, send, tryReceive)
import Specstrom.Checker.Protocol
import Specstrom.Dependency (Dep)
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Syntax (TopLevel (..))
import qualified Specstrom.Syntax as Syntax
import System.IO (hPutStrLn, isEOF, stderr)

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
      Binding b -> liftIO (Evaluator.evaluateBind env b)
      Imported _ ts' -> foldM toEnv env ts'
      Properties {} -> pure env

checkProps :: (MonadFail m, MonadIO m) => Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Analysis.AnalysisEnv -> TopLevel -> m [Result]
checkProps input output initialEnv analysisEnv = \case
  Properties _ _propGlob _ _ -> do
    let props = M.toList (M.filterWithKey (\k _ -> "prop" `Text.isPrefixOf` k) initialEnv)
    for props $ \(name, val) -> do
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      logInfo ("Checking property " <> Text.unpack name)
      checkProp input output initialEnv dep val
  _ -> pure []

data InterpreterState
  = Initial {formula :: Evaluator.Value}
  | AwaitingInitialEvent {expectedEvent :: Evaluator.BaseAction, formula :: Evaluator.Value}
  | ReadingQueue {formula :: Evaluator.Value, currentState :: State, stateVersion :: Natural}
  | AwaitingPerformed {action :: Evaluator.Action, formula :: Evaluator.Value, currentState :: State, stateVersion :: Natural}

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
    msg -> write msg >> writeStdoutFrom output

checkProp :: MonadIO m => Receive ExecutorMessage -> Send InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> m Result
checkProp input output _env dep initialFormula = do
  (valid, trace) <- runWriterT (run Initial {formula = initialFormula})
  pure (Result valid trace)
  where
    run :: InterpreterState -> Interpret m Validity
    run Initial {formula} = do
      logInfo "State: Initial"
      send output (Start dep)
      let expectedEvent = Evaluator.Loaded
      run AwaitingInitialEvent {expectedEvent, formula}
    run s@AwaitingInitialEvent {expectedEvent, formula} = do
      logInfo "State: AwaitingInitialEvent"
      msg <- receive input
      logInfo "Got initial message"
      case msg of
        Performed _state -> error "Was not expecting an action to be performed. Trace must begin with an initial event."
        Event event firstState -> do
          unless (event == expectedEvent) $
            logErr ("Initial event mismatch: " <> show event <> " =/ " <> show expectedEvent)
          tell [TraceAction (Evaluator.A event Nothing), TraceState firstState]
          run ReadingQueue {formula = formula, currentState = firstState, stateVersion = 0}
        Stale -> do
          logErr "Was not expecting a stale when awaiting initial event."
          run s
    run ReadingQueue {formula, currentState, stateVersion} = do
      logInfo "State: ReadingQueue"
      ifResidual currentState formula $ \r -> do
        msg <- tryReceive input
        case msg of
          Just (Performed _nextState) -> error "Was not awaiting a performed action"
          Just (Event event nextState) -> do
            nextFormula <- liftIO (Evaluator.step r (toEvaluatorState currentState))
            tell [TraceAction (Evaluator.A event Nothing), TraceState nextState]
            run ReadingQueue {formula = nextFormula, currentState, stateVersion = succ stateVersion}
          Just Stale -> error "Was not expecting a stale"
          Nothing ->
            case Evaluator.stop r of
              Just v -> do
                send output End
                pure (Probably v)
              Nothing -> do
                let action = Evaluator.A (Evaluator.Click "foo") Nothing -- TODO: pick action from spec based on current state
                send output (RequestAction action)
                run AwaitingPerformed {action, formula, currentState, stateVersion}
    run AwaitingPerformed {action, formula, currentState, stateVersion} = do
      logInfo "State: AwaitingPerformed"
      msg <- receive input
      case msg of
        Performed nextState ->
          run ReadingQueue {formula, currentState = nextState, stateVersion = succ stateVersion}
        Event event nextState -> do
          tell [TraceAction (Evaluator.A event Nothing), TraceState nextState]
          logErr ("Interrupted by event while awaiting " <> show action <> " to be performed. Awaiting the corresponding 'stale'.")
          -- NOTE: we could go directly to ReadingQueue here but as
          -- long as we have the Stale message, we might as well await
          -- it.
          run AwaitingPerformed {action, formula, currentState, stateVersion}
        Stale -> do
          logErr ("Got stale while awaiting " <> show action <> " to be performed.")
          run ReadingQueue {formula, currentState, stateVersion}

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
ifResidual state formula f =
  liftIO (Evaluator.force (toEvaluatorState state) formula) >>= \case
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v -> do
      logErr ("Unexpected value: " <> show v)
      pure (Probably False)
