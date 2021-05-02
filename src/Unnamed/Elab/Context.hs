module Unnamed.Elab.Context (Context, setSpan, level, empty, extend, bind) where

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

declareFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  [d|
    data Context = Context
      { span :: Span
      , env :: Env Value
      , names :: HashMap Name (Level, Value)
      , boundMask :: BoundMask
      }
      deriving stock (Show)
    |]

setSpan :: Span -> Context -> Context
setSpan span (Context _ env names mask) = Context span env names mask

level :: Context -> Level
level = view $ #env % to Env.level

empty :: Context
empty = Context (Span 0 0) Env.empty mempty BM.empty

extend :: Name -> Value -> Value -> Context -> Context
extend x a t (Context span env names mask) =
  Context span (env & Env.extend t) (names & at x ?~ (Env.level env, a)) mask

bind :: Name -> Value -> Context -> Context
bind x a (Context span env names mask) =
  Context
    span
    (env & Env.extend (V.var lvl))
    (names & at x ?~ (lvl, a))
    (mask & BM.extend lvl)
 where
  lvl = Env.level env
