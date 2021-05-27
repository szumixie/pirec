module Pirec.Syntax.Core (Term (..)) where

import Relude

import Pirec.BoundMask (BoundMask)
import Pirec.Data.MultiMap (MultiMap)
import Pirec.Data.MultiMapAlter (MultiMapAlter)
import Pirec.Label (Label)
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
  | Sigma Name Term Term
  | Pair Term Term
  | Proj1 Term
  | Proj2 Term
  | RowType Term
  | RowLit (MultiMap Label Term)
  | RowExt (MultiMap Label Term) Term
  | RecordType Term
  | RecordLit (MultiMap Label Term)
  | RecordProj Label Int Term
  | RecordAlter (MultiMapAlter Label Term) Term
  deriving stock (Show)
