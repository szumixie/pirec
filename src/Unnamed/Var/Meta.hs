module Unnamed.Var.Meta (Meta (..)) where

import Relude

import Optics (declareFieldLabels)
import Prettyprinter

declareFieldLabels
  [d|
    newtype Meta = Meta {int :: Int}
      deriving newtype (Show, Num, Eq, Hashable)
    |]

instance Pretty Meta where
  pretty (Meta i) = "?" <> pretty i
