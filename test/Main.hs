module Main where

import qualified Hedgehog.Main as Hedgehog
import Specstrom.PrettyPrinterTest as PrettyPrinterTest
import Specstrom.StreamingVerifierTest as StreamingVerifierTest

main :: IO ()
main =
  Hedgehog.defaultMain
    [ PrettyPrinterTest.tests,
      StreamingVerifierTest.tests
    ]
