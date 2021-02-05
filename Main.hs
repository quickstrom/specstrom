{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Evaluator
import Lexer
import Parser
import PrettyPrinter
import Prettyprinter.Render.Terminal (renderIO)
import System.Environment
import System.IO (hPutStr, hPutStrLn, stderr)

run :: FilePath -> Text -> IO ()
run f s = do
  case lexer (f, 1, 1) s of
    Left e -> renderIO stderr (layoutPretty defaultLayoutOptions (prettyLexerError e))
    Right ts -> case parseBindingBody builtIns ts of
      Left e -> renderIO stderr (layoutPretty defaultLayoutOptions (prettyParseError e))
      Right (_, b) -> print $ evaluate initialEnv b
  putStrLn ""

main =
  getArgs >>= \case
    [f] -> Text.readFile f >>= run f
    _ -> hPutStrLn stderr "Usage: specstrom FILE"
