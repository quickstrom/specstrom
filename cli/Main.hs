{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import Specstrom.Parser
import Specstrom.PrettyPrinter
import Specstrom.Syntax
import qualified Specstrom.Checker as Checker
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr, stdout)

searchPaths :: [FilePath]
searchPaths = ["."]

load :: Text -> IO [TopLevel]
load f = do
  result <- first prettyParseError <$> runExceptT (loadModule searchPaths ("Command line", 0, 0) f builtIns)
  case result of
    Left err -> do
      renderIO stderr (layoutPretty defaultLayoutOptions (err <> line))
      exitFailure
    Right (_, ts) -> pure ts

main :: IO ()
main =
  getArgs >>= \case
    ["format", f] -> do
      ts <- load (pack f)
      renderIO stdout (layoutPretty defaultLayoutOptions (prettyAll ts <> line))
    ["check", f] -> do
      ts <- load (pack f)
      print =<< Checker.checkAll ts
    _ -> hPutStrLn stderr "Usage: specstrom (format|check) MODULE-NAME"
