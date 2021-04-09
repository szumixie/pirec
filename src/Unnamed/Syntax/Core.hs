module Unnamed.Syntax.Core (Term (..)) where

import Relude

import Unnamed.BoundMask (BoundMask)
import Unnamed.Data.MultiMap (MultiMap)
import Unnamed.Data.MultiMapAlter (MultiMapAlter)
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
  | RowType Term
  | RowLit (MultiMap Name Term)
  | RowExt (MultiMap Name Term) Term
  | RecordType Term
  | RecordLit (MultiMap Name Term)
  | RecordProj {-# UNPACK #-} Name {-# UNPACK #-} Int Term
  | RecordAlter (MultiMapAlter Name Term) Term
  deriving stock (Show)
