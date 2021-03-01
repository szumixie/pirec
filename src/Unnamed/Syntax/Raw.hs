module Unnamed.Syntax.Raw (Term, Term' (..)) where

import Optics (declareFieldLabels)

import Unnamed.Var.Name (Name)
import Unnamed.WithPos (WithPos)

declareFieldLabels
  [d|
    type Term = WithPos Term'

    data Term'
      = Var {name :: {-# UNPACK #-} Name}
      | Let
          { name :: {-# UNPACK #-} Name
          , typ :: {-# UNPACK #-} Term
          , def :: {-# UNPACK #-} Term
          , body :: {-# UNPACK #-} Term
          }
      | U
      | Pi
          { name :: {-# UNPACK #-} Name
          , source :: {-# UNPACK #-} Term
          , target :: {-# UNPACK #-} Term
          }
      | Lam
          { name :: {-# UNPACK #-} Name
          , body :: {-# UNPACK #-} Term
          }
      | App
          { fun :: {-# UNPACK #-} Term
          , arg :: {-# UNPACK #-} Term
          }
      deriving stock (Show)
    |]
