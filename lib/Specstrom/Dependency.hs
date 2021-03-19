{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Specstrom.Dependency where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Specstrom.Syntax (Name, Selector)

newtype DepSchema = DepSchema (M.HashMap Name DepSchema)
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

-- NOTE: this right takes precendence, maybe we shouldn't use Semigroup for
-- this, but rather a custom operation that returns Either or Maybe
instance Semigroup DepSchema where
  (<>) (DepSchema a) (DepSchema b) = DepSchema (M.unionWith (<>) a b)

newtype Dep = Dep (M.HashMap Selector DepSchema)
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

instance Semigroup Dep where
  (<>) (Dep a) (Dep b) = Dep (M.unionWith (<>) a b)

instance Monoid Dep where
  mempty = Dep M.empty

project :: Name -> Dep -> Dep
project t (Dep m) = Dep (fmap (projectField t) m)
  where
    projectField n (DepSchema obj)
      | M.null obj = DepSchema (M.singleton n (DepSchema mempty))
      | otherwise = DepSchema (fmap (projectField n) obj)

dep :: Selector -> Dep
dep t = Dep (M.singleton t (DepSchema mempty))
