module Main where
import Lexer
import Parser 
import Evaluator
import PrettyPrinter
import System.Environment
import qualified Text.PrettyPrint.ANSI.Leijen as P
run :: String -> String -> IO ()
run f s = do 
  case lexer (f,1,1) s of 
    Left e -> P.putDoc (prettyLexerError e) 
    Right ts -> case parseBindingBody builtIns ts of 
      Left e -> P.putDoc (prettyParseError e)
      Right (_,b) -> print $ evaluate initialEnv b 
  putStrLn ""
  


main = getArgs >>= \[f] -> readFile f >>= run f
