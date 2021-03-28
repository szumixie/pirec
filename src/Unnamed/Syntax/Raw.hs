module Unnamed.Syntax.Raw (Term (..)) where

import Relude

import Unnamed.Data.Span (Span)
import Unnamed.Var.Name (Name)

data Term
  = Span {-# UNPACK #-} Span Term
  | Var {-# UNPACK #-} Name
  | Hole
  | Let {-# UNPACK #-} Name Term Term Term
  | U
  | Pi {-# UNPACK #-} Name Term Term
  | Lam {-# UNPACK #-} Name Term
  | App Term Term
  | RowType [Name] Term
  | RowLit [(Name, Term)]
  | RowCons [(Name, Term)] Term
  | RecordType Term
  | RecordCon [(Name, Term)]
  | RecordProj {-# UNPACK #-} Name Term
  deriving stock (Show)
