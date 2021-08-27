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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Control.Monad.Writer.Strict (tell)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn, tails)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Hedgehog (Gen, Property, annotateShow, checkParallel, cover, discover, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (CoverPercentage)
import qualified Hedgehog.Range as Range
import System.Random (randomRIO)

newtype Path = Path {unPath :: [Int]}
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

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []

type Temperature = Double

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
      case sortOn (negate <$> proximity) (unknownPaths (exploredTree result)) of
        p : _ -> Just p
        _ -> Nothing
      where
        proximity (Path p) = length (commonPrefix (unPath (highestUtilityValuePath result)) p)

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
search s uf SearchStrategy {neighbour, acceptProbability} opts@SearchOptions {maxRuns}
  | maxRuns <= 0 = fail "maxRuns must be greater than 0"
  | otherwise = do
    initialResult <- runOne mempty
    optimize initialResult (maxRuns - 1)
  where
    initialTemperature = 10

    runOne p = do
      (tree, trace) <- traverseOnce s p 10
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

type TestState = String

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
    count <- Gen.integral (Range.exponential 20 200)
    pure (map (\n -> "s" <> show n) [1 .. count :: Integer])

  let initialState = head allStates
      genState = Gen.element allStates

  let states = HashMap.fromList (zip allStates [0 ..])

  anyTransitions <-
    HashMap.fromList
      <$> sequence
        [ (state,) <$> Gen.list (Range.linear 0 (length allStates `div` 2)) (Gen.filter (/= state) genState)
          | state <- allStates
        ]

  let throughAllTransitions = HashMap.fromList [(a, [b]) | [a, b] <- windows 2 allStates]

  let transitions = HashMap.unionWith (<>) anyTransitions throughAllTransitions

  pure TestStateMachine {initialState, states, transitions}

highestValue :: TestStateMachine -> Trace TestStateMachine -> UtilityValue
highestValue sm (Trace t) = maximum [UtilityValue (HashMap.lookupDefault 0 s (states sm)) | s <- t]

approximatesGlobalMaximumWith :: SearchStrategy -> CoverPercentage -> Property
approximatesGlobalMaximumWith strategy coverage = withTests 100 . property $ do
  sm <- forAll genStateMachine
  let numStates = length (states sm)
      maxRuns = numStates `div` 4 + 1
      maxActions = numStates `div` 4
  result <- search sm (highestValue sm) strategy SearchOptions {maxRuns, maxActions}
  annotateShow result
  let maxValue = maximum (HashMap.elems (states sm))

  cover coverage "approximated highest utility value" $
    highestUtilityValue result > UtilityValue (floor (fromIntegral maxValue * 0.9 :: Double))

prop_breadth_first_simulated_annealing_approximates_global_maximum :: Property
prop_breadth_first_simulated_annealing_approximates_global_maximum = approximatesGlobalMaximumWith breadthFirstSimulatedAnnealing 65

prop_depth_first_simulated_annealing_approximates_global_maximum :: Property
prop_depth_first_simulated_annealing_approximates_global_maximum = approximatesGlobalMaximumWith depthFirstSimulatedAnnealing 50

prop_greedy_breadth_first_approximates_global_maximum :: Property
prop_greedy_breadth_first_approximates_global_maximum = approximatesGlobalMaximumWith greedyBreadthFirst 40

tests :: IO Bool
tests = checkParallel $$(discover)
