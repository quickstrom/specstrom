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
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue, TVar, STM, registerDelay, check, readTVar)
import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, (<=<), void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import qualified Specstrom.Analysis as Analysis
import Specstrom.Dependency (Dep)
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Syntax (TopLevel (..))
import qualified Specstrom.Syntax as Syntax
import System.IO (hPutStrLn, isEOF, stderr)
import System.Random (randomRIO)

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


checkAll :: [TopLevel] -> IO ()
checkAll = void . checkTopLevels [] Evaluator.basicEnv Analysis.builtIns

checkTopLevels :: [Syntax.Name] -> Evaluator.Env -> Analysis.AnalysisEnv -> [TopLevel] -> IO ([Syntax.Name], Evaluator.Env, Analysis.AnalysisEnv)
checkTopLevels currentModule evalEnv analysisEnv [] = pure (currentModule, evalEnv, analysisEnv)
checkTopLevels currentModule evalEnv analysisEnv (tl:rest) = do
  (currentModule', evalEnv', analysisEnv') <- checkTopLevel currentModule evalEnv analysisEnv tl
  checkTopLevels currentModule' evalEnv' analysisEnv' rest

checkTopLevel :: [Syntax.Name] -> Evaluator.Env -> Analysis.AnalysisEnv -> TopLevel -> IO ([Syntax.Name], Evaluator.Env, Analysis.AnalysisEnv)
checkTopLevel currentModule evalEnv analysisEnv = \case
  Binding b@(Syntax.Bind pat _) -> do 
    evalEnv' <- Evaluator.evaluateBind evalEnv b
    let analysisEnv' = Analysis.analyseBind analysisEnv b
    pure (Syntax.bindPatternBoundVars pat ++ currentModule, evalEnv', analysisEnv')
  Imported _ ts' -> do 
    (_, e', ae') <- checkTopLevels [] evalEnv analysisEnv ts'
    pure (currentModule, e', ae')
  Properties _ propGlob actsGlob initial -> do
    let props = Syntax.expand currentModule propGlob 
    let actNames = Syntax.expand currentModule actsGlob
    acts <- mapM (\ nm -> maybe (fail "Action/event not found") pure (M.lookup nm evalEnv)) actNames
    actDeps <- mapM (\ nm -> maybe (fail "Action/event dependencies not found") (pure . Analysis.depOf) (M.lookup nm analysisEnv)) actNames
    forM_ props $ \name -> do 
      val <- maybe (fail "Property not found") pure (M.lookup name evalEnv)
      dep <- Analysis.depOf <$> maybe (fail "Property dependencies not available") pure (M.lookup name analysisEnv)
      putStrLn ("Checking property: " <> Text.unpack name)
      initial' <- case initial of 
        Just e -> liftIO $ Evaluator.Thunk <$> Evaluator.newThunk evalEnv e
        Nothing -> pure (Evaluator.Action (Evaluator.A Evaluator.Loaded Nothing))
      rs <- checkProp evalEnv (mconcat (dep:actDeps)) val acts initial'
      putStrLn (show rs)
    pure (currentModule, evalEnv, analysisEnv)

data InterpreterState
  = AwaitingInitialEvent 
  | ReadingQueue {formula :: Evaluator.Residual, stateVersion :: Natural, lastState :: State, sentAction :: SentAction}

type Interpret = WriterT Trace IO


-- Read the next value from a TQueue or timeout
readTQueueTimeout :: Int -> TQueue a -> IO (Maybe a)
readTQueueTimeout timeout q = do
    delay <- registerDelay timeout
    atomically $
          Just <$> readTQueue q
      <|> Nothing <$ fini delay
  where 
    fini :: TVar Bool -> STM ()
    fini = check <=< readTVar


logErr :: MonadIO m => String -> m ()
logErr = liftIO . hPutStrLn stderr

checkProp :: Evaluator.Env -> Dep -> Evaluator.Value -> [Evaluator.Value] -> Evaluator.Value -> IO Result
checkProp env dep initialFormula actions expectedEvent = do
  input <- newTQueueIO
  output <- newTQueueIO
  Async.withAsync (readInput input) $ \inputDone ->
    Async.withAsync (writeOutput output) $ \result -> do
      checkPropOn input output env dep initialFormula actions expectedEvent
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


data SentAction = None | Sent Evaluator.Action | WaitingTimeout Int

checkPropOn :: TQueue ExecutorMessage -> TQueue InterpreterMessage -> Evaluator.Env -> Dep -> Evaluator.Value -> [Evaluator.Value] -> Evaluator.Value -> IO ()
checkPropOn input output _env dep initialFormula actions expectedEvent = do
  send (Start dep)
  (valid, trace) <- runWriterT (run AwaitingInitialEvent)
  send (Done (Result valid trace))
  where
    send :: MonadIO m => InterpreterMessage -> m ()
    send msg = liftIO (atomically (writeTQueue output msg))

    receive :: MonadIO m => m ExecutorMessage
    receive = liftIO (atomically (readTQueue input))

    tryReceive :: MonadIO m => m (Maybe ExecutorMessage)
    tryReceive = liftIO (atomically (tryReadTQueue input))

    tryReceiveTimeout :: MonadIO m => Int -> m (Maybe ExecutorMessage)
    tryReceiveTimeout i = liftIO (readTQueueTimeout i input)

    run :: InterpreterState -> Interpret Validity
    run s@AwaitingInitialEvent = do
      msg <- receive
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
                    run ReadingQueue {
                      formula = r, 
                      stateVersion = 0, 
                      lastState = firstState, 
                      sentAction = case timeout of Nothing -> None; Just i -> WaitingTimeout i
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
      msg <- case sentAction of None -> tryReceive; WaitingTimeout i -> tryReceiveTimeout i; Sent _ -> Just <$> receive
      case msg of
        Just (Performed nextState) -> case sentAction of
          Sent act@(Evaluator.A _ timeout) -> do
            nextFormula <- lift (Evaluator.step r (toEvaluatorState nextState))
            tell [TraceAction act, TraceState nextState]
            ifResidual nextState nextFormula $ \r' ->
              run ReadingQueue {
                formula = r', 
                stateVersion = succ stateVersion,  
                lastState = nextState, 
                sentAction = case timeout of Nothing -> None; Just i -> WaitingTimeout i
              }
          _ -> error "Not expecting a performed"
        Just (Event event nextState) -> do
          actvals <- liftIO $ mapM (Evaluator.force (toEvaluatorState nextState) <=< Evaluator.resetThunks) actions
          let acts = catMaybes $ flip map actvals $ \v -> case v of Evaluator.Action a -> Just a; _ -> Nothing
          let matches = filter (\(Evaluator.A a _) -> a == event) acts
          case matches of 
            [] -> run ReadingQueue {formula = r, stateVersion, lastState, sentAction}
            act@(Evaluator.A _ timeout):_ -> do
              -- TODO: Handle timeouts
              nextFormula <- lift (Evaluator.step r (toEvaluatorState nextState))
              tell [TraceAction act, TraceState nextState]
              ifResidual nextState nextFormula $ \r' ->
                run ReadingQueue {
                  formula = r', 
                  stateVersion = succ stateVersion, 
                  lastState = nextState, 
                  sentAction = case timeout of Nothing -> None; Just i -> WaitingTimeout i
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
                  idx <- randomRIO (0, len - 1)
                  let action = acts !! idx
                  send (RequestAction action)
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

ifResidual :: State -> Evaluator.Value -> (Evaluator.Residual -> Interpret Validity) -> Interpret Validity
ifResidual state formula f =
  lift (Evaluator.force (toEvaluatorState state) formula) >>= \case
    Evaluator.Trivial -> pure (Definitely True)
    Evaluator.Absurd -> pure (Definitely False)
    Evaluator.Residual r -> f r
    v -> do
      logErr ("Unexpected value: " <> show v)
      pure (Probably False)
