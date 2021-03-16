module Unnamed.Var.Meta (Meta (..)) where

import Relude

import Prettyprinter (Pretty, pretty)

newtype Meta = Meta Int
  deriving newtype (Show, Num, Eq, Hashable)

instance Pretty Meta where
  pretty (Meta i) = "?" <> pretty i
