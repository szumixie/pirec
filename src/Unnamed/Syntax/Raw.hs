module Unnamed.Syntax.Raw (Term, Term' (..)) where

import Unnamed.Var.Name (Name)
import Unnamed.WithPos (WithPos)

type Term = WithPos Term'

data Term'
  = Var {-# UNPACK #-} Name
  | Let
      {-# UNPACK #-} Name
      {-# UNPACK #-} Term
      {-# UNPACK #-} Term
      {-# UNPACK #-} Term
  | U
  | Pi {-# UNPACK #-} Name {-# UNPACK #-} Term {-# UNPACK #-} Term
  | Lam {-# UNPACK #-} Name {-# UNPACK #-} Term
  | App {-# UNPACK #-} Term {-# UNPACK #-} Term
  deriving stock (Show)
