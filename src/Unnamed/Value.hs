module Unnamed.Value (
  Closure (..),
  Value (..),
  Var (..),
  Spine (..),
  var,
  meta,
) where

import Relude

import Optics (declareFieldLabels)

import Unnamed.Env (Env)
import Unnamed.Syntax.Core (Term)
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data Closure = Closure {env :: Env Value, body :: Term}
      deriving stock (Show)

    data Value
      = Neut {var :: Var, spine :: Spine}
      | U
      | Pi
          { name :: {-# UNPACK #-} Name
          , source :: ~Value
          , target :: {-# UNPACK #-} Closure
          }
      | Lam
          { name :: {-# UNPACK #-} Name
          , body :: {-# UNPACK #-} Closure
          }
      deriving stock (Show)

    data Var
      = Rigid {level :: {-# UNPACK #-} Level}
      | Flex {meta :: {-# UNPACK #-} Meta}
      deriving stock (Show, Eq)

    data Spine
      = Nil
      | App {fun :: Spine, arg :: ~Value}
      deriving stock (Show)
    |]

var :: Level -> Value
var lx = Neut (Rigid lx) Nil

meta :: Meta -> Value
meta mx = Neut (Flex mx) Nil
