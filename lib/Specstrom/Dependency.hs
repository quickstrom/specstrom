{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Specstrom.Dependency where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import Specstrom.Syntax (Name, Selector)
import Specstrom.Action (PrimAction)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

newtype DepSchema = DepSchema (M.HashMap Name DepSchema)
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

-- NOTE: this right takes precendence, maybe we shouldn't use Semigroup for
-- this, but rather a custom operation that returns Either or Maybe
instance Semigroup DepSchema where
  (<>) (DepSchema a) (DepSchema b) = DepSchema (M.unionWith (<>) a b)

newtype Monitor = Monitor PrimAction
  deriving (Eq, Show, Generic, Hashable, JSON.ToJSON, JSON.FromJSON)

data Dep = Dep { queries :: M.HashMap Selector DepSchema, events :: HashSet PrimAction }
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

instance Semigroup Dep where
  (<>) (Dep q1 a1) (Dep q2 a2) = Dep (M.unionWith (<>) q1 q2) (a1 <> a2)

instance Monoid Dep where
  mempty = Dep mempty mempty

project :: Name -> Dep -> Dep
project t (Dep q a) = Dep (fmap (projectField t) q) a
  where
    projectField n (DepSchema obj)
      | M.null obj = DepSchema (M.singleton n (DepSchema mempty))
      | otherwise = DepSchema (fmap (projectField n) obj)

queryDep :: Selector -> Dep
queryDep t = Dep (M.singleton t (DepSchema mempty)) mempty

actionDep :: PrimAction -> Dep
actionDep a = Dep mempty (HashSet.singleton a)
