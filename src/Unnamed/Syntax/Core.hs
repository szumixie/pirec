module Unnamed.Syntax.Core (Term (..)) where

import Relude

import Optics (declareFieldLabels)

import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data Term
      = Var {level :: {-# UNPACK #-} Level}
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
      deriving stock (Show)
    |]
