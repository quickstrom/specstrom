{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Specstrom.Checker.Protocol where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import Specstrom.Dependency (Dep)
import qualified Specstrom.Evaluator as Evaluator
import qualified Specstrom.Syntax as Syntax

type Trace = [TraceElement]

type State = M.HashMap Syntax.Selector [JSON.Value]

data TraceElement = TraceAction Evaluator.Action | TraceState State
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data Validity = Definitely Bool | Probably Bool
  deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

data Result = Result {valid :: Validity, trace :: Trace}
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data InterpreterMessage
  = Start {dependencies :: Dep}
  | End
  | RequestAction {action :: Evaluator.Action}
  | Done {results :: [Result]}
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data ExecutorMessage
  = Performed State
  | Event Evaluator.BaseAction State
  | Stale
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)
