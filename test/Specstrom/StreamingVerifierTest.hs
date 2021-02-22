{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Specstrom.StreamingVerifierTest where

import Hedgehog (Gen, Property, annotateShow, checkParallel, discover, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Specstrom.StreamingVerifier
import qualified Hedgehog.Range as Range
import qualified Pipes

prop_always_eventually_equivalence_list :: Property
prop_always_eventually_equivalence_list = property $ do
  trace <- forAll genTrace
  state <- forAll genState
  n <- forAll (Gen.integral (Range.linear 0 10))
  let p = is state
  case (verifyList (always n p) trace, verifyList (formulaNegate (eventually n (formulaNegate p))) trace) of
    (Right r1, Right r2) -> r1 === r2
    (Left CannotStep{}, Left CannotStep{}) -> pure ()
    (Left UnexpectedEndFormula{}, _) -> failure
    (_, Left UnexpectedEndFormula{}) -> failure
    (r1, r2) -> annotateShow (r1, r2) >> failure

prop_always_eventually_equivalence_stream :: Property
prop_always_eventually_equivalence_stream = property $ do
  trace <- forAll genTrace
  state <- forAll genState
  n <- forAll (Gen.integral (Range.linear 0 10))
  let p = is state
  case (verify (always n p) (Pipes.each trace), verify (formulaNegate (eventually n (formulaNegate p))) (Pipes.each trace)) of
    (Right r1, Right r2) -> r1 === r2
    (Left CannotStep{}, Left CannotStep{}) -> pure ()
    (Left UnexpectedEndFormula{}, _) -> failure
    (_, Left UnexpectedEndFormula{}) -> failure
    (r1, r2) -> annotateShow (r1, r2) >> failure

genTrace :: Gen Trace
genTrace = Gen.string (Range.linear 1 20) genState 

genState :: Gen State
genState = Gen.element "abc"

tests :: IO Bool
tests = checkParallel $$(discover)
