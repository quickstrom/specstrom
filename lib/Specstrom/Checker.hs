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
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Foldable (Foldable (fold, foldl'))
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import qualified Specstrom.Analysis as Analysis
import Specstrom.Dependency (Dep)
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Syntax (TopLevel (..))
import qualified Specstrom.Syntax as Syntax
import System.IO (hPutStrLn, isEOF, stderr)

type Trace = [TraceElement]

type State = M.HashMap Syntax.Selector [JSON.Value]

data TraceElement = TraceAction Evaluator.Action | TraceState State
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data Validity = Definitely Bool | Probably Bool
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data Result = Result {valid :: Validity, trace :: Trace}
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data InterpreterMessage
  = Start {dependencies :: Dep}
  | End
  | RequestAction {action :: Evaluator.Action}
  | Done {result :: Result}
  deriving (Generic, JSON.ToJSON, JSON.FromJSON)

data ExecutorMessage
  = Performed State
  | Event Evaluator.BaseAction State
  | Stale
  deriving (Generic, JSON.ToJSON, JSON.FromJSON)

checkAll :: [TopLevel] -> IO [Result]
checkAll ts = do
  env <- foldM toEnv Evaluator.basicEnv ts
  let analysisEnv = foldl' toAnalysisEnv Analysis.builtIns ts
  fold <$> traverse (checkProps env analysisEnv) ts
  where
    toEnv :: Evaluator.Env -> TopLevel -> IO Evaluator.Env
    toEnv env = \case
      Binding b -> Evaluator.evaluateBind env b
      Imported _ ts' -> foldM toEnv env ts'
      Properties {} -> pure env
    toAnalysisEnv :: Analysis.AnalysisEnv -> TopLevel -> Analysis.AnalysisEnv
    toAnalysisEnv env = \case
      Binding b -> Analysis.analyseBind env b
      Imported _ ts' -> foldl' toAnalysisEnv env ts'
      Properties {} -> env

checkProps :: Evaluator.Env -> Analysis.AnalysisEnv -> TopLevel -> IO [Result]
checkProps initialEnv analysisEnv = \case
  Properties _ _propGlob _ _ -> do
    let props = M.toList (M.filterWithKey (\k _ -> "prop" `Text.isPrefixOf` k) initialEnv)
    for props $ \(name, val) -> do
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      putStrLn ("Checking property: " <> Text.unpack name)
      checkProp initialEnv dep val (Evaluator.Action (Evaluator.A Evaluator.Loaded Nothing))
  _ -> pure []

data InterpreterState
  = AwaitingInitialEvent {expectedEvent :: Evaluator.Value, formula :: Evaluator.Value}
  | ReadingQueue {formula :: Evaluator.Value, currentState :: State, stateVersion :: Natural}
  | AwaitingPerformed {action :: Evaluator.Action, formula :: Evaluator.Value, currentState :: State, stateVersion :: Natural}

type Interpret = WriterT Trace IO

logErr :: MonadIO m => String -> m ()
logErr = liftIO . hPutStrLn stderr

checkProp :: Evaluator.Env -> Dep -> Evaluator.Value -> Evaluator.Value -> IO Result
checkProp env dep initialFormula expectedEvent = do
  input <- newTQueueIO
  output <- newTQueueIO
  Async.withAsync (readInput input) $ \inputDone ->
    Async.withAsync (writeOutput output) $ \result -> do
      checkPropOn input output env dep initialFormula expectedEvent
      Async.wait inputDone
      Async.wait result

readInput :: TQueue ExecutorMessage -> IO ()
readInput input = do
  eof <- isEOF
  unless eof $ do
    line <- getLine
    case JSON.eitherDecodeStrict (BS.pack line) of
      Left err -> logErr ("Input message parsing failed: " <> err)
      Right msg -> atomically (writeTQueue input msg)
    readInput input

writeOutput :: TQueue InterpreterMessage -> IO Result
writeOutput output = do
  let write msg = LBS.putStrLn (JSON.encode msg)
  atomically (readTQueue output) >>= \case
    Done result -> write (Done result) $> result
    msg -> write msg >> writeOutput output

checkPropOn :: TQueue ExecutorMessage -> TQueue InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> Evaluator.Value -> IO ()
checkPropOn input output _env dep initialFormula expectedEv = do
  send (Start dep)
  (valid, trace) <- runWriterT (run AwaitingInitialEvent {formula = initialFormula, expectedEvent = expectedEv })
  send (Done (Result valid trace))
  where
    send :: MonadIO m => InterpreterMessage -> m ()
    send msg = liftIO (atomically (writeTQueue output msg))

    receive :: MonadIO m => m ExecutorMessage
    receive = liftIO (atomically (readTQueue input))

    tryReceive :: MonadIO m => m (Maybe ExecutorMessage)
    tryReceive = liftIO (atomically (tryReadTQueue input))

    run :: InterpreterState -> Interpret Validity    
    run s@AwaitingInitialEvent {expectedEvent, formula} = do
      msg <- receive
      case msg of
        Performed _state -> error "Was not expecting an action to be performed. Trace must begin with an initial event."
        Event event firstState -> do
          expected <- liftIO $ Evaluator.force (toEvaluatorState firstState) expectedEvent 
          case expected of 
            Evaluator.Action (Evaluator.A base _timeout) -> 
              if base == event then do
                tell [TraceAction (Evaluator.A event Nothing), TraceState firstState]
                run ReadingQueue {formula = formula, currentState = firstState, stateVersion = 0}
              else do
                -- maybe want to log this
                expectedEvent' <- liftIO $ Evaluator.resetThunks expectedEvent                
                run (AwaitingInitialEvent {expectedEvent = expectedEvent', formula})
            _ -> error "Provided initial event is not an action"
        Stale -> do
          logErr "Was not expecting a stale when awaiting initial event."
          run s
    run ReadingQueue {formula, currentState, stateVersion} =
      ifResidual currentState formula $ \r -> do
        msg <- tryReceive
        case msg of
          Just (Performed _nextState) -> error "Was not awaiting a performed action"
          Just (Event event nextState) -> do
            nextFormula <- lift (Evaluator.step r (toEvaluatorState currentState))
            tell [TraceAction (Evaluator.A event Nothing), TraceState nextState]
            run ReadingQueue {formula = nextFormula, currentState, stateVersion = succ stateVersion}
          Just Stale -> error "Was not expecting a stale"
          Nothing ->
            case Evaluator.stop r of
              Just v -> pure (Probably v)
              Nothing -> do
                let action = Evaluator.A (Evaluator.Click "foo") Nothing -- TODO: pick action from spec based on current state
                send (RequestAction action)
                run AwaitingPerformed {action, formula, currentState, stateVersion}
    run AwaitingPerformed {action, formula, currentState, stateVersion} = do
      msg <- receive
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

ifResidual :: State -> Evaluator.Value -> (Evaluator.Residual -> Interpret Validity) -> Interpret Validity
ifResidual state formula f =
  lift (Evaluator.force (toEvaluatorState state) formula) >>= \case
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v -> do
      logErr ("Unexpected value: " <> show v)
      pure (Probably False)
