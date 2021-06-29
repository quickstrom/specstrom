{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specstrom.AnalysisTest where

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.String
import qualified Specstrom.Analysis as Analysis
import qualified Specstrom.Parser as Parser
import Specstrom.PrettyPrinter (prettyParseError)
import qualified Specstrom.Syntax as Syntax

test :: IO ()
test = do
  ts <- load "test/Specstrom/AnalysisTest/events"
  let dep = foldMap Analysis.depOf (Analysis.analyseTopLevels ts)
  -- TODO:
  -- 1. convert this to a runnable test
  -- 2. check that the event deps are correct
  print dep

load :: Text -> IO [Syntax.TopLevel]
load f = do
  let searchPaths = [".", "ulib"]
  result <- runExceptT (Parser.loadModule searchPaths (Text.unpack f, 0, 0) f Parser.builtIns)
  case result of
    Left err -> fail (renderString (Doc.layoutPretty Doc.defaultLayoutOptions (prettyParseError err)))
    Right (_, ts) -> pure ts