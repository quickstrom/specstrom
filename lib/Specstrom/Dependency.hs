{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Specstrom.Dependency where

import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON

newtype Dep = Dep (M.Map Text [[Text]])
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

instance Semigroup Dep where
  (<>) (Dep a) (Dep b) = Dep (M.unionWith (++) a b)

instance Monoid Dep where
  mempty = Dep M.empty

project :: Text -> Dep -> Dep
project t (Dep m) = Dep (fmap (map (++ [t])) m)

dep :: Text -> Dep
dep t = Dep (M.fromList [(t, [[]])])
