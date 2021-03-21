module Main where

import qualified Hedgehog.Main as Hedgehog
import Specstrom.PrettyPrinterTest as PrettyPrinterTest
import Specstrom.CheckerTest as CheckerTest

main :: IO ()
main =
  Hedgehog.defaultMain
    [ PrettyPrinterTest.tests,
      CheckerTest.tests
    ]
