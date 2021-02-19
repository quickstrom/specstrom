{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Specstrom.StreamingVerifierTest where

import Hedgehog (Gen, Property, annotateShow, checkParallel, discover, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Specstrom.StreamingVerifier
import qualified Hedgehog.Range as Range
import Algebra.Heyting (Heyting(neg))

prop_always_eventually_equivalence :: Property
prop_always_eventually_equivalence = property $ do
  trace <- forAll genTrace
  state <- forAll genState
  n <- forAll (Gen.integral (Range.linear 0 10))
  let p = is state
  case (verify (always n p) trace, verify (neg (eventually n (neg p))) trace) of
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
