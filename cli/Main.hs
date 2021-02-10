{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.Terminal (renderIO)
import Specstrom.Evaluator
import Specstrom.Lexer
import Specstrom.Parser
import Specstrom.PrettyPrinter
import System.Environment
import System.IO (hPutStrLn, stderr, stdout)

run :: FilePath -> Text -> IO ()
run f s =
  case run' of
    Left err -> renderIO stderr (layoutPretty defaultLayoutOptions (err <> line))
    Right doc -> renderIO stdout (layoutPretty defaultLayoutOptions (doc <> line))
  where
    run' = do
      ts <- first prettyLexerError (lexer (f, 1, 1) s)
      (_, b) <- first prettyParseError (parseBindingBody builtIns ts)
      formula <- first prettyEvalError (evaluate initialEnv b)
      return (prettyValue formula)

main :: IO ()
main =
  getArgs >>= \case
    [f] -> Text.readFile f >>= run f
    _ -> hPutStrLn stderr "Usage: specstrom FILE"
