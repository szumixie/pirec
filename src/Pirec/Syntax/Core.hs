module Pirec.Syntax.Core (Term (..)) where

import Relude

import Pirec.BoundMask (BoundMask)
import Pirec.Data.MultiMap (MultiMap)
import Pirec.Data.MultiMapAlter (MultiMapAlter)
import Pirec.Plicity (Plicity)
import Pirec.Var.Level (Level)
import Pirec.Var.Meta (Meta)
import Pirec.Var.Name (Name)

data Term
  = Var Level
  | Meta Meta (Maybe BoundMask)
  | Let Name Term Term
  | Univ
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
