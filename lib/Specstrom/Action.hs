{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Specstrom.Action where

import qualified Data.Aeson as JSON
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Specstrom.Syntax as Syntax

data PrimAction = A {id :: Syntax.Name, isEvent :: Bool, args :: [JSON.Value], timeout :: Maybe Int}
  deriving (Show, Eq, Generic, Hashable, JSON.ToJSON, JSON.FromJSON)