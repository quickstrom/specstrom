{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Specstrom.CheckerTest where

import qualified Control.Concurrent.Async as Async
import Control.Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens as JSON
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.String
import Hedgehog (Property, annotateShow, checkParallel, discover, evalIO, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Specstrom.Analysis as Analysis
import qualified Specstrom.Channel as Channel
import qualified Specstrom.Checker as Checker
import qualified Specstrom.Checker.Protocol as Protocol
import qualified Specstrom.Gen as Gen
import qualified Specstrom.Parser as Parser
import Specstrom.PrettyPrinter (prettyParseError)
import qualified Specstrom.Syntax as Syntax

prop_check_produces_result :: Property
prop_check_produces_result = property $ do
  ts <- evalIO (load "test/Specstrom/CheckerTest/next")
  let dep = foldMap Analysis.depOf (Analysis.analyseTopLevels ts)
  annotateShow dep
  states <- forAll (Gen.list (Range.linear 2 10) (enableButtons <$> Gen.state dep))
  (interpreterRecv, interpreterSend) <- Channel.newChannel
  (executorRecv, executorSend) <- Channel.newChannel
  results <- evalIO $
    Async.withAsync (Checker.checkAll executorRecv interpreterSend ts) $ \interpreter ->
      Async.withAsync (runSessions interpreterRecv executorSend states) $ \executor -> do
        Async.wait interpreter
        Async.wait executor
  annotateShow results

enableButtons :: Protocol.State -> Protocol.State
enableButtons = traverse . JSON._Array . traverse . JSON._Object . at "disabled" .~ pure (JSON.Bool True)

runSessions ::
  MonadIO m =>
  Channel.Receive Protocol.InterpreterMessage ->
  Channel.Send Protocol.ExecutorMessage ->
  [Protocol.State] ->
  m [Protocol.Result]
runSessions input output = go
  where
    go states = do
      msg <- Channel.receive input
      case (msg, states) of
        (Protocol.RequestAction {}, s : ss) -> do
          Channel.send output (Protocol.Performed s)
          go ss
        (Protocol.Start {}, s : ss) -> do
          Channel.send
            output
            ( Protocol.Event
                Protocol.A
                  { Protocol.id = "loaded",
                    Protocol.isEvent = True,
                    Protocol.args = [],
                    Protocol.timeout = Nothing
                  }
                s
            )
          go ss
        (Protocol.End {}, _) -> go states
        (Protocol.Done results, _) -> pure results
        _ -> error ("Unexpected: " <> show (msg, states))

load :: Text -> IO [Syntax.TopLevel]
load f = do
  let searchPaths = ["."]
  result <- runExceptT (Parser.loadModule searchPaths ("Command line", 0, 0) f Parser.builtIns)
  case result of
    Left err -> fail (renderString (Doc.layoutPretty Doc.defaultLayoutOptions (prettyParseError err)))
    Right (_, ts) -> pure ts

tests :: IO Bool
tests = checkParallel $$(discover)
