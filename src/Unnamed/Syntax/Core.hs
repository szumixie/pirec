module Unnamed.Syntax.Core (Term (..)) where

import Relude

import Unnamed.BoundMask (BoundMask)
import Unnamed.LabelSet (LabelSet)
import Unnamed.Var.Level (Level)
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

data Term
  = Var {-# UNPACK #-} Level
  | Meta {-# UNPACK #-} Meta (Maybe BoundMask)
  | Let {-# UNPACK #-} Name Term Term
  | U
  | Pi {-# UNPACK #-} Name Term Term
  | Lam {-# UNPACK #-} Name Term
  | App Term Term
  | RowType LabelSet Term
  | RowLit (HashMap Name Term)
  | RowCons (HashMap Name Term) Term
  | RecordType Term
  | RecordLit (HashMap Name Term)
  | RecordProj {-# UNPACK #-} Name Term
  | RecordMod {-# UNPACK #-} Name Term Term
  deriving stock (Show)
