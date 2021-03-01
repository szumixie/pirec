module Unnamed.Var.Level (Level (..)) where

import Data.Hashable (Hashable)

import Optics (declareFieldLabels)

declareFieldLabels
  [d|
    newtype Level = Level {int :: Int}
      deriving newtype (Show, Num, Eq, Hashable, Ord)
    |]
