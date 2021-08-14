{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Specstrom.TargetedSearchTest where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Control.Monad.Writer.Strict (tell)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Hedgehog (Gen, Property, annotateShow, checkParallel, discover, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Random (randomRIO)

newtype Path = Path [Int]
  deriving (Eq, Show, Semigroup, Monoid)

data RoseTree a = Node (NonEmpty (RoseTree a)) | Leaf a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data PathTreeState = Terminal | Unknown
  deriving (Eq, Show)

leafPathsMatching :: (a -> Bool) -> RoseTree a -> [Path]
leafPathsMatching predicate root = go [(mempty, root)] []
  where
    go [] acc = acc
    go ((path, tree) : rest) acc =
      case tree of
        Node children ->
          let newTrees = zipWith (\i child -> (path <> Path (pure i), child)) [0 .. (length children - 1)] (NonEmpty.toList children)
           in go (newTrees <> rest) acc
        Leaf a
          | predicate a -> go rest (acc <> [path])
          | otherwise -> go rest acc

type PathTree = RoseTree PathTreeState

merge :: PathTree -> PathTree -> PathTree
merge (Node c1) (Node c2) = Node (NonEmpty.zipWith merge c1 c2)
merge (Node c) _ = Node c
merge _ (Node c) = Node c
merge (Leaf Unknown) n = n
merge n (Leaf Unknown) = n
merge (Leaf Terminal) (Leaf Terminal) = Leaf Terminal

unknownPaths :: PathTree -> [Path]
unknownPaths = leafPathsMatching (== Unknown)

terminalPaths :: PathTree -> [Path]
terminalPaths = leafPathsMatching (== Terminal)

class Monad m => StateSpace s m where
  type State s :: *
  type Action s :: *

  initial :: s -> m (State s)
  actions :: s -> State s -> m [Action s]
  perform :: s -> State s -> Action s -> m (State s)

newtype Trace s = Trace [State s]
  deriving (Semigroup, Monoid)

newtype UtilityValue = UtilityValue Int
  deriving (Eq, Ord, Show)

type UtilityFunction s = Trace s -> UtilityValue

data OptimizationResult = OptimizationResult
  { exploredTree :: PathTree,
    highestUtilityValue :: UtilityValue,
    highestUtilityValuePath :: Path
  }
  deriving (Eq, Show)

data SearchOptions = SearchOptions {maxRuns :: Int, maxActions :: Int}

traverseOnce :: MonadIO m => StateSpace s m => s -> Path -> Int -> m (PathTree, Trace s)
traverseOnce s path maxActions = do
  runWriterT $ do
    firstState <- reportState =<< lift (initial s)
    go firstState path maxActions
  where
    unknowns n = replicate n (Leaf Unknown)
    reportState state =
      tell (Trace [state]) >> pure state
    go currentState _ 0 = pure (Leaf Terminal)
    go currentState (Path currentBasePath) actionsLeft = do
      lift (actions s currentState) >>= \case
        [] -> pure (Leaf Terminal)
        actions' -> do
          (actionIndex, remainingPath) <- case currentBasePath of
            [] -> (,[]) <$> randomRIO (0, length actions' - 1)
            (first : rest) -> pure (first, rest)
          newState <- reportState =<< lift (perform s currentState (actions' !! actionIndex))
          result <- go newState (Path remainingPath) (actionsLeft - 1)
          pure (Node (NonEmpty.fromList (unknowns actionIndex <> [result] <> unknowns (length actions' - actionIndex - 1))))

search :: MonadIO m => StateSpace s m => s -> UtilityFunction s -> SearchOptions -> m OptimizationResult
search s uf opts@SearchOptions {maxRuns} = do
  initialResult <- runOne mempty
  optimize initialResult (maxRuns - 1)
  where
    runOne p = do
      (tree, trace) <- traverseOnce s p 10
      let uv = uf trace
      pure (OptimizationResult {exploredTree = tree, highestUtilityValue = uv, highestUtilityValuePath = head (terminalPaths tree)})

    optimize oldResult 0 = pure oldResult
    optimize oldResult i = do
      case unknownPaths (exploredTree oldResult) of
        [] -> pure oldResult
        candidateBasePath : _ -> do
          candidateResult <- runOne candidateBasePath
          let mergedTree = merge (exploredTree oldResult) (exploredTree candidateResult)

          let newResult = (if highestUtilityValue oldResult < highestUtilityValue candidateResult then candidateResult else oldResult) {exploredTree = mergedTree}
          optimize newResult (i - 1)

data TestStateMachine = TestStateMachine
  { initialState :: Char,
    states :: HashMap Char Int,
    transitions :: HashMap Char [Char]
  }
  deriving (Eq, Show)

instance (MonadIO m, MonadFail m) => StateSpace TestStateMachine m where
  type Action TestStateMachine = Int
  type State TestStateMachine = Char
  initial TestStateMachine {initialState} = pure initialState
  actions TestStateMachine {transitions} state = do
    as <- maybe (fail "Invalid state") pure (HashMap.lookup state transitions)
    pure [0 .. length as - 1]
  perform TestStateMachine {transitions} state i = do
    ts <- maybe (fail "Invalid state") pure (HashMap.lookup state transitions)
    pure (ts !! i)

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . tails

possibleStates :: [Char]
possibleStates = ['a' .. 'z']

genStateMachine :: Gen TestStateMachine
genStateMachine = do
  allStates <- do
    n <- Gen.integral (Range.linear 2 (length possibleStates - 1))
    pure (take n possibleStates)

  let initialState = head allStates
      genState = Gen.element allStates

  states <-
    HashMap.fromList
      <$> sequence [(state,) <$> Gen.integral (Range.linear 0 10) | state <- allStates]

  anyTransitions <-
    HashMap.fromList
      <$> sequence [(state,) <$> Gen.list (Range.linear 0 10) genState | state <- allStates]

  let throughAllTransitions = HashMap.fromList [(a, [b]) | [a, b] <- windows 2 allStates]

  let transitions = HashMap.unionWith (<>) anyTransitions throughAllTransitions

  pure TestStateMachine {initialState, states, transitions}

highestValue :: TestStateMachine -> Trace TestStateMachine -> UtilityValue
highestValue sm (Trace t) = maximum [UtilityValue (HashMap.lookupDefault 0 s (states sm)) | s <- t]

prop_prettyprint_parse_roundtrip :: Property
prop_prettyprint_parse_roundtrip = property $ do
  sm <- forAll genStateMachine
  let numStates = length (states sm)
  result <- search sm (highestValue sm) SearchOptions {maxRuns = numStates * 10, maxActions = numStates * 10}
  annotateShow result
  let maxValue = maximum (HashMap.elems (states sm))
  highestUtilityValue result === UtilityValue maxValue

tests :: IO Bool
tests = checkParallel $$(discover)
