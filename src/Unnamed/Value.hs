module Unnamed.Value (
  Closure (..),
  Value (..),
  Neutral (..),
  var,
  app,
) where

import Unnamed.Env (Env)
import Unnamed.Syntax.Core (Term)
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Name (Name)

data Closure = Closure (Env Value) Term
  deriving stock (Show)

data Value
  = Neut Neutral
  | U
  | Pi {-# UNPACK #-} Name ~Value {-# UNPACK #-} Closure
  | Lam {-# UNPACK #-} Name {-# UNPACK #-} Closure
  deriving stock (Show)

data Neutral
  = Var {-# UNPACK #-} Level
  | App Neutral ~Value
  deriving stock (Show)

var :: Level -> Value
var = Neut . Var

app :: Neutral -> Value -> Value
app t u = Neut $ App t u
