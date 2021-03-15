module Unnamed.Eval (appClosure, openClosure, eval, quote, normal) where

import Data.Maybe (fromMaybe)

import Optics

import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Value (Closure, Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)

appClosure :: Closure -> Value -> Value
appClosure (V.Closure env t) u = eval (env & Env.extend u) t

openClosure :: Level -> Closure -> Value
openClosure lvl t = appClosure t (V.var lvl)

eval :: Env Value -> Term -> Value
eval !env = go
 where
  go = \case
    Var x -> env & Env.index x & fromMaybe (error "bug")
    Let _ _ t u -> eval (env & Env.extend (go t)) u
    U -> V.U
    Pi x a b -> V.Pi x (go a) $ V.Closure env b
    Lam x t -> V.Lam x $ V.Closure env t
    App t u -> case go t of
      V.Neut t' -> V.app t' (go u)
      V.Lam _ t' -> appClosure t' (go u)
      _ -> error "bug"
    RowType a -> V.RowType $ go a
    RowCon ts -> V.RowCon $ go <$> ts
    RecordType r -> V.RecordType $ go r
    RecordCon ts -> V.RecordCon $ go <$> ts
    RecordProj f t -> case go t of
      V.Neut t' -> V.Neut $ V.RecordProj f t'
      V.RecordCon ts -> ts ^. at f & fromMaybe (error "bug")
      _ -> error "bug"

quote :: Level -> Value -> Term
quote !lvl = go
 where
  go = \case
    V.Neut t -> goNeut t
    V.U -> U
    V.Pi x a b -> Pi x (go a) $ quote (lvl + 1) (openClosure lvl b)
    V.Lam x t -> Lam x $ quote (lvl + 1) (openClosure lvl t)
    V.RowType a -> RowType $ go a
    V.RowCon ts -> RowCon $ go <$> ts
    V.RecordType r -> RecordType $ go r
    V.RecordCon ts -> RecordCon $ go <$> ts

  goNeut = \case
    V.Var x -> Var x
    V.App t u -> App (goNeut t) (go u)
    V.RecordProj f t -> RecordProj f (goNeut t)

normal :: Term -> Term
normal = quote 0 . eval Env.empty
