{-# LANGUAGE ScopedTypeVariables #-}
module Specstrom.PrettyPrinterTest where

import Hedgehog (property, Property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_prettyprint_format_roundtrip :: Property
hprop_prettyprint_format_roundtrip = property $ do
  n :: Int <- forAll (Gen.integral (Range.linear 0 10))
  n + n === n * 2