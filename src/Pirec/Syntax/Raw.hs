module Pirec.Syntax.Raw (Term (..)) where

import Relude

import Pirec.Data.Span (Span)
import Pirec.Label (Label)
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
  | RowExt Label Term Term
  | RecordType Term
  | RecordEmpty
  | RecordExt Label (Maybe Term) Term Term
  | RecordProj Label Term
  | RecordRestr Label Term
  deriving stock (Show)
