{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (first)
import Data.Text (Text,pack)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import Specstrom.Evaluator
import Specstrom.Lexer
import Specstrom.Parser
import Specstrom.PrettyPrinter
import System.Environment
import System.IO (hPutStrLn, stderr, stdout)
import Control.Monad.Except (runExceptT)

searchPaths :: [FilePath]
searchPaths = ["."]

load :: Text -> IO ()
load f = do
  result <- first prettyParseError <$> runExceptT (loadModule searchPaths ("Command line",0,0) f builtIns)
  case result of
    Left err -> renderIO stderr (layoutPretty defaultLayoutOptions (err <> line))
    Right (_,doc) -> renderIO stdout (layoutPretty defaultLayoutOptions (prettyAll doc <> line))

main :: IO ()
main =
  getArgs >>= \case
    [f] -> load (pack f)
    _ -> hPutStrLn stderr "Usage: specstrom MODULE-NAME"
