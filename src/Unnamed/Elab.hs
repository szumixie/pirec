module Unnamed.Elab (check, infer) where

import Unnamed.Elab.Context (Context)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Elab.Error
import Unnamed.Eval
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Syntax.Raw qualified as R
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.WithPos (WithPos (..), withPos)

conv :: Level -> Value -> Value -> Bool
conv !lvl = go
 where
  go = curry \case
    (V.Neut t, V.Neut t') -> goNeut t t'
    (V.U, V.U) -> True
    (V.Pi _ a b, V.Pi _ a' b') ->
      go a a' && conv (lvl + 1) (openClosure lvl b) (openClosure lvl b')
    (V.Lam _ t, V.Lam _ t') ->
      conv (lvl + 1) (openClosure lvl t) (openClosure lvl t')
    (V.Lam _ t, V.Neut t') ->
      conv (lvl + 1) (openClosure lvl t) (V.app t' (V.var lvl))
    (V.Neut t, V.Lam _ t') ->
      conv (lvl + 1) (V.app t (V.var lvl)) (openClosure lvl t')
    _ -> False

  goNeut = curry \case
    (V.Var x, V.Var x') -> x == x'
    (V.App t u, V.App t' u') -> goNeut t t' && go u u'
    _ -> False

type ElabM = Either ElabError

check :: Context -> R.Term -> Value -> ElabM Term
check = fst . checkInfer

infer :: Context -> R.Term -> ElabM (Term, Value)
infer = snd . checkInfer

checkInfer ::
  Context -> (R.Term -> Value -> ElabM Term, R.Term -> ElabM (Term, Value))
checkInfer !ctx = (goInfer, goCheck)
 where
  goInfer = withPos \pos -> curry \case
    (R.Let x a t u, b) -> do
      a' <- goInfer a V.U
      let va = eval (Ctx.env ctx) a'
      t' <- goInfer t va
      let vt = eval (Ctx.env ctx) t'
      u' <- check (Ctx.extend x va vt ctx) u b
      pure $ Let x a' t' u'
    (R.Lam x t, V.Pi _ a b) ->
      Lam x <$> check (Ctx.bind x a ctx) t (openClosure (Ctx.level ctx) b)
    (t, a) -> do
      (t', a') <- goCheck $ WithPos pos t
      if conv (Ctx.level ctx) a a'
        then pure t'
        else Left $ ElabError pos ctx (ConvError a a')

  goCheck = withPos \pos -> \case
    R.Var x -> case Ctx.getType x ctx of
      Nothing -> Left $ ElabError pos ctx (ScopeError x)
      Just (l, t) -> Right (Var l, t)
    R.Let x a t u -> do
      a' <- goInfer a V.U
      let va = eval (Ctx.env ctx) a'
      t' <- goInfer t va
      let vt = eval (Ctx.env ctx) t'
      (u', b) <- infer (Ctx.extend x va vt ctx) u
      pure (Let x a' t' u', b)
    R.U -> pure (U, V.U)
    R.Pi x a b -> do
      a' <- goInfer a V.U
      let va = eval (Ctx.env ctx) a'
      b' <- check (Ctx.bind x va ctx) b V.U
      pure (Pi x a' b', V.U)
    R.Lam{} -> Left $ ElabError pos ctx LamInference
    R.App t u -> do
      (t', c) <- goCheck t
      case c of
        V.Pi _ a b -> do
          u' <- goInfer u a
          pure (App t' u', appClosure b (eval (Ctx.env ctx) u'))
        _ -> Left $ ElabError pos ctx (PiExpected c)
