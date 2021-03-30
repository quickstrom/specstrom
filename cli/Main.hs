{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text (pack)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import qualified Specstrom.Checker as Checker
import Specstrom.PrettyPrinter
import System.Environment
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import REPL(repl)
import Util


main :: IO ()
main =
  getArgs >>= \case
    ["format", f] -> do
      ts <- load (pack f)
      renderIO stdout (layoutPretty defaultLayoutOptions (prettyAll ts <> line))
    ["repl", f] -> do
      repl (pack f)
    ["check", f] -> do
      ts <- load (pack f)
      hSetBuffering stderr LineBuffering
      hSetBuffering stdout LineBuffering
      Checker.checkAllStdio ts
    _ -> hPutStrLn stderr "Usage: specstrom (format|check|repl) MODULE-NAME"
