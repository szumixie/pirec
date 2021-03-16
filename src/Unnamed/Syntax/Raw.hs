module Unnamed.Syntax.Raw (Term, Term' (..)) where

import Relude

import Unnamed.Var.Name (Name)
import Unnamed.WithPos (WithPos)

type Term = WithPos Term'

data Term'
  = Var {-# UNPACK #-} Name
  | Hole
  | Let
      {-# UNPACK #-} Name
      {-# UNPACK #-} Term
      {-# UNPACK #-} Term
      {-# UNPACK #-} Term
  | U
  | Pi {-# UNPACK #-} Name {-# UNPACK #-} Term {-# UNPACK #-} Term
  | Lam {-# UNPACK #-} Name {-# UNPACK #-} Term
  | App {-# UNPACK #-} Term {-# UNPACK #-} Term
  | RowType {-# UNPACK #-} Term
  | RowCon [(Name, Term)]
  | RecordType {-# UNPACK #-} Term
  | RecordCon [(Name, Term)]
  | RecordProj {-# UNPACK #-} Name {-# UNPACK #-} Term
  deriving stock (Show)
