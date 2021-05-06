module Unnamed.Plicity (Plicity (..)) where

import Prettyprinter (Pretty, pretty)
import Relude

data Plicity
  = Explicit
  | Implicit
  deriving stock (Show, Eq)

instance Pretty Plicity where
  pretty = \case
    Explicit -> "explicit"
    Implicit -> "implicit"
