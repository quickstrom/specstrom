module Main where

import qualified Hedgehog.Main as Hedgehog
import Specstrom.PrettyPrinterTest as PrettyPrinterTest

main :: IO ()
main =
  Hedgehog.defaultMain
    [ PrettyPrinterTest.tests
    ]
