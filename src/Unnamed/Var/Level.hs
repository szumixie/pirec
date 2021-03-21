module Unnamed.Var.Level (Level (..)) where

import Relude

import Optics (declareFieldLabels)

declareFieldLabels
  [d|
    newtype Level = Level {int :: Int}
      deriving newtype (Show, Num, Eq, Hashable, Ord)
    |]
