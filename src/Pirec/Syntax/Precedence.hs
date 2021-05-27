module Pirec.Syntax.Precedence (Precedence (..)) where

import Relude

data Precedence
  = Arrow
  | Times
  | Comma
  | App
  | Proj
  | Atom
  deriving stock (Show, Eq, Ord, Enum, Bounded)
