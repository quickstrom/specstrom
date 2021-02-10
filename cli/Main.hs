{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (renderIO)
import Specstrom.Evaluator
import Specstrom.Lexer
import Specstrom.Parser
import Specstrom.PrettyPrinter
import System.Environment
import System.IO (hPutStrLn, stderr)

run :: FilePath -> Text -> IO ()
run f s = do
  case lexer (f, 1, 1) s of
    Left e -> renderIO stderr (layoutPretty defaultLayoutOptions (prettyLexerError e))
    Right ts -> case parseBindingBody builtIns ts of
      Left e -> renderIO stderr (layoutPretty defaultLayoutOptions (prettyParseError e))
      Right (_, b) -> print $ evaluate initialEnv b
  putStrLn ""

main :: IO ()
main =
  getArgs >>= \case
    [f] -> Text.readFile f >>= run f
    _ -> hPutStrLn stderr "Usage: specstrom FILE"
