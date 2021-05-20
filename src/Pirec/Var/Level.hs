module Pirec.Var.Level (Level (..)) where

import Relude

newtype Level = Level Int
  deriving newtype (Show, Num, Eq, Hashable, Ord, Enum)
