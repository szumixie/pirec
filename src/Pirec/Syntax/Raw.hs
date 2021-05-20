module Pirec.Syntax.Raw (Term (..)) where

import Relude

import Pirec.Data.Span (Span)
import Pirec.Plicity (Plicity)
import Pirec.Var.Name (Name)

data Term
  = Span Span Term
  | Var Name
  | Hole
  | Let Name (Maybe Term) Term Term
  | Univ
  | Pi Plicity Name (Maybe Term) Term
  | Lam Plicity Name (Maybe Term) Term
  | App Plicity Term Term
  | RowType Term
  | RowEmpty
  | RowExt Name Term Term
  | RecordType Term
  | RecordEmpty
  | RecordExt Name (Maybe Term) Term Term
  | RecordProj Name Term
  | RecordRestr Name Term
  deriving stock (Show)
