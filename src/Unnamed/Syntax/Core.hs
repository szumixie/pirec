module Unnamed.Syntax.Core (Term (..)) where

import Relude

import Unnamed.BoundMask (BoundMask)
import Unnamed.Data.MultiMap (MultiMap)
import Unnamed.Data.MultiMapAlter (MultiMapAlter)
import Unnamed.Plicity (Plicity)
import Unnamed.Var.Level (Level)
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

data Term
  = Var Level
  | Meta Meta (Maybe BoundMask)
  | Let Name Term Term
  | U
  | Pi Plicity Name Term Term
  | Lam Plicity Name Term
  | App Plicity Term Term
  | RowType Term
  | RowLit (MultiMap Name Term)
  | RowExt (MultiMap Name Term) Term
  | RecordType Term
  | RecordLit (MultiMap Name Term)
  | RecordProj Name Int Term
  | RecordAlter (MultiMapAlter Name Term) Term
  deriving stock (Show)
