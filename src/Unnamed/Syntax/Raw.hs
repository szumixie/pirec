module Unnamed.Syntax.Raw (Term (..)) where

import Relude

import Unnamed.Data.Span (Span)
import Unnamed.Var.Name (Name)

data Term
  = Span Span Term
  | Var Name
  | Hole
  | Let Name (Maybe Term) Term Term
  | U
  | Pi Name (Maybe Term) Term
  | Lam Name (Maybe Term) Term
  | App Term Term
  | RowType Term
  | RowEmpty
  | RowExt Name Term Term
  | RecordType Term
  | RecordEmpty
  | RecordExt Name (Maybe Term) Term Term
  | RecordProj Name Term
  | RecordRestr Name Term
  deriving stock (Show)
