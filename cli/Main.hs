{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty, line)
import Options.Applicative
import Prettyprinter.Render.Terminal (renderIO)
import REPL (repl)
import qualified Specstrom.Checker as Checker
import Specstrom.Load
import Specstrom.PrettyPrinter
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

data CliOptions = CliOptions
  { searchPaths :: [FilePath],
    cliCommand :: Command
  }
  deriving (Show)

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> many (strOption (long "include" <> short 'I' <> help "include the given path in module search paths" <> metavar "DIR"))
    <*> subparser
      ( command "format" (info formatOptionsParser (progDesc "Format the given module"))
          <> command "repl" (info replOptionsParser (progDesc "Start a REPL session loading the given module"))
          <> command "check" (info checkOptionsParser (progDesc "Check the given module (should be run by an executor according to the check protocol)"))
      )

data Command
  = Format FormatOptions
  | Repl ReplOptions
  | Check CheckOptions
  deriving (Show)

data FormatOptions = FormatOptions
  { formatModule :: Text
  }
  deriving (Show)

formatOptionsParser :: Parser Command
formatOptionsParser = Format . FormatOptions <$> argument str (metavar "MODULE")

data ReplOptions = ReplOptions
  { replModule :: Text
  }
  deriving (Show)

replOptionsParser :: Parser Command
replOptionsParser = Repl . ReplOptions <$> argument str (metavar "MODULE")

data CheckOptions = CheckOptions
  { checkModule :: Text
  }
  deriving (Show)

checkOptionsParser :: Parser Command
checkOptionsParser = Check . CheckOptions <$> argument str (metavar "MODULE")

parserInfo :: ParserInfo CliOptions
parserInfo =
  info
    (cliOptionsParser <**> helper)
    ( fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative"
    )

main :: IO ()
main = do
  cliOpts <- execParser parserInfo
  let allSearchPaths = searchPaths cliOpts <> ["."]
  case cliCommand cliOpts of
    Format (FormatOptions f) -> do
      load allSearchPaths f >>= \case
        Left err -> do
          renderIO stdout (layoutPretty defaultLayoutOptions (prettyLoadError err <> line))
          exitFailure
        Right ts -> renderIO stdout (layoutPretty defaultLayoutOptions (prettyAll ts <> line))
    Repl (ReplOptions f) -> do
      repl allSearchPaths f
    Check (CheckOptions f) -> do
      hSetBuffering stderr LineBuffering
      hSetBuffering stdout LineBuffering
      result <- load allSearchPaths f
      Checker.checkAllStdio result
