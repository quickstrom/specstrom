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
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Specstrom.TargetedSearchTest where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Control.Monad.Writer.Strict (tell, when)
import qualified Data.Graph as Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort, sortOn, tails)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import GHC.Stack (withFrozenCallStack)
import Hedgehog (Gen, MonadTest, Property, annotateShow, assert, checkParallel, classify, cover, discard, discover, forAll, label, property, withTests, withDiscards)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (CoverPercentage, LabelName (LabelName))
import qualified Hedgehog.Range as Range
import System.Random (randomRIO)

newtype Path = Path {unPath :: [Int]}
  deriving (Eq, Show, Semigroup, Monoid)

leafPathsMatching :: (a -> Bool) -> Graph.Tree a -> [Path]
leafPathsMatching predicate root = go [(mempty, root)] []
  where
    go [] acc = acc
    go ((path, tree) : rest) acc =
      case tree of
        Graph.Node a []
          | predicate a -> go rest (acc <> [path])
          | otherwise -> go rest acc
        Graph.Node _ children ->
          let newTrees = zipWith (\i child -> (path <> Path (pure i), child)) [0 .. (length children - 1)] children
           in go (newTrees <> rest) acc

data PathTreeState = Branch | Terminal | Unknown
  deriving (Eq, Show)

type PathTree = Graph.Tree PathTreeState

merge :: PathTree -> PathTree -> PathTree
merge (Graph.Node Unknown _) n = n
merge n (Graph.Node Unknown _) = n
merge (Graph.Node Terminal _) (Graph.Node Terminal _) = Graph.Node Terminal []
merge (Graph.Node Branch c1) (Graph.Node Branch c2) = Graph.Node Branch (zipWith merge c1 c2)
merge (Graph.Node s c) _ = Graph.Node s c

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

newtype UtilityValue = UtilityValue {unUtilityValue :: Int}
  deriving (Eq, Ord, Show)

type UtilityFunction s = Trace s -> UtilityValue

data OptimizationResult = OptimizationResult
  { exploredTree :: PathTree,
    highestUtilityValue :: UtilityValue,
    highestUtilityValuePath :: Path
  }
  deriving (Eq, Show)

data SearchOptions = SearchOptions {maxRuns :: Int, maxActions :: Int}
  deriving (Eq, Show)

data SearchStrategy = SearchStrategy
  { -- | Generates a new path to try, based on the current optimization result, if possible.
    neighbour :: OptimizationResult -> Maybe Path,
    -- | Returns the probability [0, 1] of accepting a new candidate. This function is
    -- called 'P' in the SA literature.
    acceptProbability :: OptimizationResult -> OptimizationResult -> Temperature -> Double
  }

traverseOnce :: MonadIO m => StateSpace s m => s -> Path -> Int -> m (PathTree, Trace s)
traverseOnce s path maxActions = do
  runWriterT $ do
    firstState <- reportState =<< lift (initial s)
    go firstState path maxActions
  where
    unknowns n = replicate n (Graph.Node Unknown [])
    reportState state =
      tell (Trace [state]) >> pure state
    go currentState _ 0 = pure (Graph.Node Terminal [])
    go currentState (Path currentBasePath) actionsLeft = do
      lift (actions s currentState) >>= \case
        [] -> pure (Graph.Node Terminal [])
        actions' -> do
          (actionIndex, remainingPath) <- case currentBasePath of
            [] -> (,[]) <$> randomRIO (0, length actions' - 1)
            (first : rest) -> pure (first, rest)
          newState <- reportState =<< lift (perform s currentState (actions' !! actionIndex))
          result <- go newState (Path remainingPath) (actionsLeft - 1)
          pure (Graph.Node Branch (unknowns actionIndex <> [result] <> unknowns (length actions' - actionIndex - 1)))

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []

type Temperature = Double

fullyRandom :: SearchStrategy
fullyRandom = SearchStrategy {neighbour, acceptProbability}
  where
    neighbour _ = mempty
    acceptProbability old new t = 0

greedyBreadthFirst :: SearchStrategy
greedyBreadthFirst = SearchStrategy {neighbour, acceptProbability}
  where
    neighbour result =
      case sortOn pathLength (unknownPaths (exploredTree result)) of
        p : _ -> Just p
        _ -> Nothing
    pathLength (Path p) = negate (length p)
    acceptProbability old new t = 0

breadthFirstSimulatedAnnealing :: SearchStrategy
breadthFirstSimulatedAnnealing = SearchStrategy {neighbour, acceptProbability}
  where
    neighbour result =
      case unknownPaths (exploredTree result) of
        p : _ -> Just p
        _ -> Nothing

    acceptProbability old new t =
      let UtilityValue uv = highestUtilityValue old
          UtilityValue uv' = highestUtilityValue new
       in if uv' > uv then 1 else exp (negate (fromIntegral (uv - uv') / t))

depthFirstSimulatedAnnealing :: SearchStrategy
depthFirstSimulatedAnnealing = SearchStrategy {neighbour, acceptProbability}
  where
    neighbour result =
      case sortOn proximity (unknownPaths (exploredTree result)) of
        p : _ -> Just p
        _ -> Nothing
      where
        proximity (Path p) = negate (similarity p * length p)
        similarity p = length (commonPrefix (unPath (highestUtilityValuePath result)) p)

    acceptProbability old new t =
      let UtilityValue uv = highestUtilityValue old
          UtilityValue uv' = highestUtilityValue new
       in if uv' > uv then 1 else exp (negate (fromIntegral (uv - uv') / t))

search ::
  (MonadFail m, MonadIO m) =>
  StateSpace s m =>
  s ->
  UtilityFunction s ->
  SearchStrategy ->
  SearchOptions ->
  m OptimizationResult
search s uf SearchStrategy {neighbour, acceptProbability} opts@SearchOptions {maxRuns, maxActions}
  | maxRuns <= 0 = fail "maxRuns must be greater than 0"
  | otherwise = do
    initialResult <- runOne mempty
    optimize initialResult (maxRuns - 1)
  where
    initialTemperature = 10

    runOne p = do
      (tree, trace) <- traverseOnce s p maxActions
      let uv = uf trace
      pure (OptimizationResult {exploredTree = tree, highestUtilityValue = uv, highestUtilityValuePath = head (terminalPaths tree)})

    optimize oldResult 0 = pure oldResult
    optimize oldResult i = do
      case neighbour oldResult of
        Nothing -> pure oldResult
        Just candidateBasePath -> do
          candidateResult <- runOne candidateBasePath
          let mergedTree = merge (exploredTree oldResult) (exploredTree candidateResult)
          n <- randomRIO (0, 1)
          let currentTemp = initialTemperature * (fromIntegral i / fromIntegral maxRuns)
              newResult =
                ( if acceptProbability oldResult candidateResult currentTemp >= n
                    then candidateResult
                    else oldResult
                )
                  { exploredTree = mergedTree
                  }
          optimize newResult (i - 1)

------------------------------------------------------------------

-- * Tests

------------------------------------------------------------------

type TestState = Int

data TestStateMachine = TestStateMachine
  { initialState :: TestState,
    states :: HashMap TestState Int,
    transitions :: HashMap TestState [TestState]
  }
  deriving (Eq, Show)

instance (MonadIO m, MonadFail m) => StateSpace TestStateMachine m where
  type Action TestStateMachine = Int
  type State TestStateMachine = TestState
  initial TestStateMachine {initialState} = pure initialState
  actions TestStateMachine {transitions} state = do
    as <- maybe (fail "Invalid state") pure (HashMap.lookup state transitions)
    pure [0 .. length as - 1]
  perform TestStateMachine {transitions} state i = do
    ts <- maybe (fail "Invalid state") pure (HashMap.lookup state transitions)
    pure (ts !! i)

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . tails

genStateMachine :: Gen TestStateMachine
genStateMachine = do
  allStates <- do
    count <- Gen.integral (Range.linear 200 500)
    pure [1..count]

  let initialState = head allStates
      genStatesAbove x range = case [succ x .. last allStates] of
        [] -> pure []
        xs -> Gen.list range (Gen.element xs)
      genState = Gen.element allStates

  let states = HashMap.fromList (zip allStates allStates)

  anyTransitions <-
    HashMap.fromList
      <$> sequence
        [ (state,) <$>  genStatesAbove state (Range.linear 2 5)
          | state <- allStates
        ]

  transitionsToHighestValue <- do
    pathStates <- Gen.set (Range.linear 5 10) genState
    pure (HashMap.fromList [(a, [b]) | [a, b] <- windows 2 (sort (last allStates : Set.toList pathStates))])

  let transitions = HashMap.unionWith (<>) anyTransitions transitionsToHighestValue

  pure TestStateMachine {initialState, states, transitions}

highestValue :: TestStateMachine -> UtilityFunction TestStateMachine
highestValue sm (Trace t) = maximum [UtilityValue (HashMap.lookupDefault 0 s (states sm)) | s <- t]

sumValue :: TestStateMachine -> UtilityFunction TestStateMachine
sumValue sm (Trace t) = UtilityValue (sum [HashMap.lookupDefault 0 s (states sm) | s <- t])

longestTrace :: UtilityFunction s
longestTrace (Trace t) = UtilityValue (length t)

annotateShow' :: (MonadTest m, Show a) => m a -> m a
annotateShow' ma = withFrozenCallStack $ do
  a <- ma
  annotateShow a
  pure a

findsGlobalMaximumWith :: SearchStrategy -> CoverPercentage -> Property
findsGlobalMaximumWith strategy coverage = withTests 100 . property $ do
  sm <- forAll genStateMachine
  let maxRuns = 5
      maxActions = 5
  result <- annotateShow' (search sm (highestValue sm) strategy SearchOptions {maxRuns, maxActions})
  let maxValue = maximum (HashMap.elems (states sm))

  cover coverage "found highest utility value" $
    highestUtilityValue result == UtilityValue maxValue

prop_fully_random_finds_global_maximum :: Property
prop_fully_random_finds_global_maximum = findsGlobalMaximumWith fullyRandom 20

prop_breadth_first_simulated_annealing_finds_global_maximum :: Property
prop_breadth_first_simulated_annealing_finds_global_maximum = findsGlobalMaximumWith breadthFirstSimulatedAnnealing 30

prop_depth_first_simulated_annealing_finds_global_maximum :: Property
prop_depth_first_simulated_annealing_finds_global_maximum = findsGlobalMaximumWith depthFirstSimulatedAnnealing 40

prop_greedy_breadth_first_finds_global_maximum :: Property
prop_greedy_breadth_first_finds_global_maximum = findsGlobalMaximumWith greedyBreadthFirst 30

prop_best_strategy :: Property
prop_best_strategy = withDiscards 100 . withTests 100 . property $ do
  sm <- forAll genStateMachine
  let maxRuns = 5
      maxActions = 5
      opts = SearchOptions {maxRuns, maxActions}
      maxValue = maximum (HashMap.elems (states sm))

      x `beats` y = highestUtilityValue x > highestUtilityValue y

  resultRandom <- annotateShow' (search sm (highestValue sm) fullyRandom opts)
  resultBFSA <- annotateShow' (search sm (highestValue sm) breadthFirstSimulatedAnnealing opts)
  resultDFSA <- annotateShow' (search sm (highestValue sm) depthFirstSimulatedAnnealing opts)
  resultGreedy <- annotateShow' (search sm (highestValue sm) greedyBreadthFirst opts)

  when (highestUtilityValue resultRandom == UtilityValue maxValue) discard

  cover 50 "breadth-first SA beats random" $
    resultBFSA `beats` resultRandom
 
  cover 50 "depth-first SA beats random" $
    resultDFSA `beats` resultRandom

  cover 30 "breadth-first SA beats breadth-first greedy" $
    resultBFSA `beats` resultGreedy

prop_max_state_is_reachable :: Property
prop_max_state_is_reachable = withTests 100 . property $ do
  sm <- forAll genStateMachine
  let maxState = maximum (HashMap.elems (states sm))
      g =
        Graph.buildG
          (1, maxState)
          [ (from, to)
            | (from, tos) <- HashMap.toList (transitions sm),
              to <- tos
          ]

  assert (Graph.path g 1 maxState)

tests :: IO Bool
tests = checkParallel $$(discover)
