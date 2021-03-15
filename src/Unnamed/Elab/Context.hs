module Unnamed.Elab.Context (Context, empty, extend, bind) where

import Relude hiding (empty)

import Optics

import Unnamed.BoundMask (BoundMask)
import Unnamed.BoundMask qualified as BM
import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

declareFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  [d|
    data Context = Context
      { level :: {-# UNPACK #-} Level
      , env :: Env Value
      , names :: HashMap Name (Level, Value)
      , boundMask :: BoundMask
      }
      deriving stock (Show)
    |]

empty :: Context
empty = Context 0 Env.empty mempty BM.empty

extend :: Name -> Value -> Value -> Context -> Context
extend x va vt (Context lvl env names mask) =
  Context
    (lvl + 1)
    (env & Env.extend vt)
    (names & at x ?~ (lvl, va))
    mask

bind :: Name -> Value -> Context -> Context
bind x va (Context lvl env names mask) =
  Context
    (lvl + 1)
    (env & Env.extend (V.var lvl))
    (names & at x ?~ (lvl, va))
    (mask & BM.extend lvl)
