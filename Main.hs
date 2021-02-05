{-# LANGUAGE LambdaCase #-}
module Main where

import Evaluator
import Lexer
import Parser
import PrettyPrinter
import System.Environment
import qualified Text.PrettyPrint.ANSI.Leijen as P
import System.IO (hPutStrLn, stderr, hPutStr)

run :: String -> String -> IO ()
run f s = do
  case lexer (f, 1, 1) s of
    Left e -> P.hPutDoc stderr (prettyLexerError e)
    Right ts -> case parseBindingBody builtIns ts of
      Left e -> P.hPutDoc stderr (prettyParseError e)
      Right (_, b) -> print $ evaluate initialEnv b
  putStrLn ""

main = getArgs >>= \case
  [f] -> readFile f >>= run f
  _ -> hPutStrLn stderr "Usage: specstrom FILE"
