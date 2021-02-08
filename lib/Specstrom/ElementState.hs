module Specstrom.ElementState where

import Data.Text (Text)
data ElementState
  = Attribute Text
  | Property Text
  | CssValue Text
  | Text
  deriving (Eq, Ord, Show)