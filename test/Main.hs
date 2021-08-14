module Main where

import qualified Hedgehog.Main as Hedgehog
import Specstrom.CheckerTest as CheckerTest
import Specstrom.PrettyPrinterTest as PrettyPrinterTest
import Specstrom.TargetedSearchTest as TargetedSearchTest

main :: IO ()
main =
  Hedgehog.defaultMain
    [ PrettyPrinterTest.tests,
      CheckerTest.tests,
      TargetedSearchTest.tests
    ]
