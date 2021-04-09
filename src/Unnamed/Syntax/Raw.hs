module Unnamed.Syntax.Raw (Term (..)) where

import Relude

import Unnamed.Data.Span (Span)
import Unnamed.Var.Name (Name)

data Term
  = Span {-# UNPACK #-} Span Term
  | Var {-# UNPACK #-} Name
  | Hole
  | Let {-# UNPACK #-} Name (Maybe Term) Term Term
  | U
  | Pi {-# UNPACK #-} Name Term Term
  | Lam {-# UNPACK #-} Name (Maybe Term) Term
  | App Term Term
  | RowType Term
  | RowEmpty
  | RowExt {-# UNPACK #-} Name Term Term
  | RecordType Term
  | RecordEmpty
  | RecordExt {-# UNPACK #-} Name Term Term
  | RecordProj {-# UNPACK #-} Name Term
  | RecordRestr {-# UNPACK #-} Name Term
  deriving stock (Show)
