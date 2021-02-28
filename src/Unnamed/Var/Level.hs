module Unnamed.Var.Level (Level (..)) where

import Data.Hashable (Hashable)

newtype Level = Level Int
  deriving newtype (Show, Num, Eq, Hashable, Ord)
