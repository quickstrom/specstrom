{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Specstrom.Checker.Protocol where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Specstrom.Dependency (Dep)
import qualified Specstrom.Syntax as Syntax

type Trace = [TraceElement]

type State = M.HashMap Syntax.Selector [JSON.Value]

data PrimAction = A {id :: Syntax.Name, isEvent :: Bool, args :: [JSON.Value], timeout :: Maybe Int}
  deriving (Show, Eq, Generic, JSON.ToJSON, JSON.FromJSON)

actionMatches :: PrimAction -> PrimAction -> Bool
actionMatches p q = p {timeout = Nothing} == q {timeout = Nothing}

maximumTimeout :: [PrimAction] -> Maybe Int
maximumTimeout as = case catMaybes (map timeout as) of
  [] -> Nothing
  ls -> Just (maximum ls)

data TraceElement = TraceAction [PrimAction] | TraceState State
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data Validity = Definitely Bool | Probably Bool
  deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

data Result = Result {valid :: Validity, trace :: Trace}
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data InterpreterMessage
  = Start {dependencies :: Dep}
  | End
  | RequestAction {action :: PrimAction}
  | Done {results :: [Result]}
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)

data ExecutorMessage
  = Performed State
  | Event PrimAction State
  | Stale
  deriving (Show, Generic, JSON.ToJSON, JSON.FromJSON)
