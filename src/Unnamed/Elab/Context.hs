module Unnamed.Elab.Context (
  Context,
  span,
  env,
  level,
  getName,
  prettyCtx,
  boundMask,
  empty,
  extend,
  bind,
  insert,
) where

import Relude hiding (empty, span)

import Optics

import Unnamed.BoundMask (BoundMask)
import Unnamed.BoundMask qualified as BM
import Unnamed.Data.Span (Span (..))
import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Pretty.Context qualified as Pretty
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

declareLenses
  [d|
    data Context = Context
      { _span :: Span
      , _env :: Env Value
      , _names :: HashMap Name (Level, Value)
      , _prettyCtx :: Pretty.Context
      , _boundMask :: BoundMask
      }
      deriving stock (Show)
    |]

span :: Lens' Context Span
span = _span

env :: Context -> Env Value
env = view _env

level :: Context -> Level
level = Env.level . env

getName :: Name -> Context -> Maybe (Level, Value)
getName x = view $ _names % at x

prettyCtx :: Context -> Pretty.Context
prettyCtx = view _prettyCtx

boundMask :: Context -> BoundMask
boundMask = view _boundMask

empty :: Context
empty = Context (Span 0 0) Env.empty mempty Pretty.empty BM.empty

extend :: Name -> Value -> Value -> Context -> Context
extend x a t ctx =
  ctx
    & _env %~ Env.extend t
    & _names % at x ?~ (level ctx, a)
    & _prettyCtx %~ Pretty.extend x

bind :: Name -> Value -> Context -> Context
bind x a ctx =
  ctx & extend x a (V.var $ level ctx) & _boundMask %~ BM.extend (level ctx)

insert :: Name -> Context -> Context
insert x ctx =
  ctx
    & _env %~ Env.extend (V.var $ level ctx)
    & _prettyCtx %~ Pretty.extend x
    & _boundMask %~ BM.extend (level ctx)
