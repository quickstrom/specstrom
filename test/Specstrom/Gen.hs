module Specstrom.Gen where

import Hedgehog (Gen)
import Specstrom.Evaluator (Accessor, Formula(..))
import qualified Hedgehog.Gen as Gen

genFormula :: Gen (Formula Accessor)
genFormula = Gen.element [Trivial, Absurd]