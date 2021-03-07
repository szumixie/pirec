module Unnamed.Elab.Context (Context (..), empty, extend, bind) where

import Relude hiding (empty)

import Data.HashMap.Strict qualified as Map

import Optics

import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data Context = Context
      { level :: {-# UNPACK #-} Level
      , env :: Env Value
      , names :: HashMap Name (Level, Value)
      }
      deriving stock (Show)
    |]

empty :: Context
empty = Context 0 Env.empty Map.empty

extend :: Name -> Value -> Value -> Context -> Context
extend x a t (Context lvl env names) =
  Context (lvl + 1) (env & Env.extend t) (names & at x ?~ (lvl, a))

bind :: Name -> Value -> Context -> Context
bind x a ctx@(Context lvl _ _) = ctx & extend x a (V.var lvl)
