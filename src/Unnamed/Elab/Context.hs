module Unnamed.Elab.Context (Context, empty, extend, bind) where

import Relude hiding (empty)

import Optics

import Unnamed.BoundMask (BoundMask)
import Unnamed.BoundMask qualified as BM
import Unnamed.Data.Span (Span (..))
import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data Context = Context
      { span :: Span
      , level :: {-# UNPACK #-} Level
      , env :: Env Value
      , names :: HashMap Name (Level, Value)
      , boundMask :: BoundMask
      }
      deriving stock (Show)
    |]

empty :: Context
empty = Context (Span 0 0) 0 Env.empty mempty BM.empty

extend :: Name -> Value -> Value -> Context -> Context
extend x va vt (Context sp lvl env names mask) =
  Context
    sp
    (lvl + 1)
    (env & Env.extend vt)
    (names & at x ?~ (lvl, va))
    mask

bind :: Name -> Value -> Context -> Context
bind x va (Context sp lvl env names mask) =
  Context
    sp
    (lvl + 1)
    (env & Env.extend (V.var lvl))
    (names & at x ?~ (lvl, va))
    (mask & BM.extend lvl)
