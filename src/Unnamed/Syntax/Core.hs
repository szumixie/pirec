module Unnamed.Syntax.Core (Term (..)) where

import Relude

import Optics (declareFieldLabels)

import Unnamed.BoundMask (BoundMask)
import Unnamed.Var.Level (Level)
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data Term
      = Var {level :: {-# UNPACK #-} Level}
      | Meta {meta :: {-# UNPACK #-} Meta, boundMask :: Maybe BoundMask}
      | Let
          { name :: {-# UNPACK #-} Name
          , typ :: Term
          , def :: Term
          , body :: Term
          }
      | U
      | Pi
          { name :: {-# UNPACK #-} Name
          , source :: Term
          , target :: Term
          }
      | Lam
          { name :: {-# UNPACK #-} Name
          , body :: Term
          }
      | App
          { fun :: Term
          , arg :: Term
          }
      | RowType {typ :: Term}
      | RowCon {elems :: HashMap Name Term}
      | RecordType {row :: Term}
      | RecordCon {elems :: HashMap Name Term}
      | RecordProj {field :: {-# UNPACK #-} Name, record :: Term}
      deriving stock (Show)
    |]
