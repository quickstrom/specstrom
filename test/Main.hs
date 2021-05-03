module Main where

import qualified Hedgehog.Main as Hedgehog
import Specstrom.CheckerTest as CheckerTest
import Specstrom.PrettyPrinterTest as PrettyPrinterTest

main :: IO ()
main =
  Hedgehog.defaultMain
    [ PrettyPrinterTest.tests,
      CheckerTest.tests
    ]
