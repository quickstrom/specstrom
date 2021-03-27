{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Specstrom.CheckerTest where

import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.String
import Hedgehog (Property, annotateShow, checkParallel, discover, evalIO, property, failure, forAll, (===), assert)
import qualified Specstrom.Channel as Channel
import qualified Specstrom.Checker as Checker
import qualified Specstrom.Checker.Protocol as Protocol
import qualified Specstrom.Evaluator as Evaluator
import qualified Specstrom.Parser as Parser
import Specstrom.PrettyPrinter (prettyParseError)
import qualified Specstrom.Syntax as Syntax
import qualified Specstrom.Analysis as Analysis
import qualified Specstrom.Gen as Gen
import Data.Either (isRight)

prop_check_produces_result :: Property
prop_check_produces_result = property $ do
  ts <- evalIO (load "test/Specstrom/CheckerTest/next")
  let dep = foldMap Analysis.depOf (Analysis.analyseTopLevels ts)
  annotateShow dep
  state <- forAll (Gen.state dep)
  (interpreterRecv, interpreterSend) <- Channel.newChannel
  (executorRecv, executorSend) <- Channel.newChannel
  let send = Channel.send executorSend
  results <- evalIO $
    Async.withAsync (Checker.checkAll executorRecv interpreterSend ts) $ \checker -> do
      let runSessions = do
            Channel.receive interpreterRecv >>= \case
                Protocol.RequestAction {} -> do
                  Channel.send executorSend (Protocol.Performed state)
                  runSessions
                Protocol.Start{} -> do
                  send (Protocol.Event Evaluator.Loaded state)
                  runSessions
                Protocol.End{} -> runSessions
                Protocol.Done{} -> pure ()
      runSessions
      Async.waitCatch checker
  annotateShow results
  assert (isRight results)

load :: Text -> IO [Syntax.TopLevel]
load f = do
  let searchPaths = ["."]
  result <- runExceptT (Parser.loadModule searchPaths ("Command line", 0, 0) f Parser.builtIns)
  case result of
    Left err -> fail (renderString (Doc.layoutPretty Doc.defaultLayoutOptions (prettyParseError err)))
    Right (_, ts) -> pure ts

tests :: IO Bool
tests = checkParallel $$(discover)
