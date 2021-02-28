module Unnamed.Syntax.Core (Term (..)) where

import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

data Term
  = Var {-# UNPACK #-} Level
  | Let {-# UNPACK #-} Name Term Term Term
  | U
  | Pi {-# UNPACK #-} Name Term Term
  | Lam {-# UNPACK #-} Name Term
  | App Term Term
  deriving stock (Show)
