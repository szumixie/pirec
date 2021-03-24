module Unnamed.Elab (check, infer) where

import Relude

import Control.Effect
import Control.Effect.Error
import Optics

import Unnamed.Syntax.Core (Term (..))
import Unnamed.Syntax.Raw qualified as R
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Name (Name)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Elab.Error
import Unnamed.Eval
import Unnamed.Unify (unify)

insertMeta :: Eff MetaState m => Context -> m Term
insertMeta ctx = do
  meta <- freshMeta
  pure $ Meta meta (Just $ ctx ^. #boundMask)

elabUnify ::
  Effs [MetaCtx, Throw ElabError] m => Context -> Value -> Value -> m ()
elabUnify ctx vt vt' =
  throwToThrow (ElabError ctx . UnifyError vt vt') $
    unify (ctx ^. #level) vt vt'

check ::
  Effs [MetaCtx, Throw ElabError] m => Context -> R.Term -> Value -> m Term
check = fst . checkInfer

infer ::
  Effs [MetaCtx, Throw ElabError] m => Context -> R.Term -> m (Term, Value)
infer = snd . checkInfer

checkInfer ::
  Effs [MetaCtx, Throw ElabError] m =>
  Context ->
  (R.Term -> Value -> m Term, R.Term -> m (Term, Value))
checkInfer ctx = (goCheck, goInfer)
 where
  goCheck = curry \case
    (R.Span span rt, va) -> check (ctx & #span .~ span) rt va
    (R.Hole, _) -> insertMeta ctx
    (R.Let x ra rt ru, vb) -> do
      a <- goCheck ra V.U
      va <- eval (ctx ^. #env) a
      t <- goCheck rt va
      vt <- eval (ctx ^. #env) t
      u <- check (ctx & Ctx.extend x va vt) ru vb
      pure $ Let x a t u
    (R.Lam x rt, V.Pi _ va closure) -> do
      vb <- openClosure (ctx ^. #level) closure
      Lam x <$> check (ctx & Ctx.bind x va) rt vb
    (rt, va) -> do
      (t, va') <- goInfer rt
      t <$ elabUnify ctx va va'

  goInfer = \case
    R.Span span rt -> infer (ctx & #span .~ span) rt
    R.Var x -> case ctx ^. #names % at x of
      Nothing -> throw $ ElabError ctx (ScopeError x)
      Just (lx, va) -> pure (Var lx, va)
    R.Hole -> (,) <$> insertMeta ctx <*> (eval (ctx ^. #env) =<< insertMeta ctx)
    R.Let x ra rt ru -> do
      a <- goCheck ra V.U
      va <- eval (ctx ^. #env) a
      t <- goCheck rt va
      vt <- eval (ctx ^. #env) t
      (u, vb) <- infer (ctx & Ctx.extend x va vt) ru
      pure (Let x a t u, vb)
    R.U -> pure (U, V.U)
    R.Pi x ra rb -> do
      a <- goCheck ra V.U
      va <- eval (ctx ^. #env) a
      b <- check (ctx & Ctx.bind x va) rb V.U
      pure (Pi x a b, V.U)
    R.Lam x rt -> do
      va <- eval (ctx ^. #env) =<< insertMeta ctx
      (t, vb) <- infer (ctx & Ctx.bind x va) rt
      closure <- closeValue (ctx ^. #env) vb
      pure (Lam x t, V.Pi x va closure)
    R.App rt ru -> do
      (t, vp0) <- goInfer rt
      (va, closure) <-
        forceValue vp0 >>= \case
          V.Pi _ va vb -> pure (va, vb)
          vp -> do
            va <- eval (ctx ^. #env) =<< insertMeta ctx
            closure <-
              V.Closure (ctx ^. #env) <$> insertMeta (ctx & Ctx.bind "x" va)
            (va, closure) <$ elabUnify ctx vp (V.Pi "x" va closure)
      u <- goCheck ru va
      vu <- eval (ctx ^. #env) u
      vb <- appClosure closure vu
      pure (App t u, vb)
    R.RowType ra -> do
      a <- goCheck ra V.U
      pure (RowType a, V.U)
    R.RowCon [] -> do
      va <- eval (ctx ^. #env) =<< insertMeta ctx
      pure (RowCon mempty, va)
    R.RowCon ((x1, rt1) : rts) -> do
      (t1, va) <- goInfer rt1
      rts' <- labelsUnique ctx ((x1, rt1) : rts)
      ts <- ifor rts' \x t -> if x == x1 then pure t1 else goCheck t va
      pure (RowCon ts, V.RowType va)
    R.RecordType rr -> do
      r <- goCheck rr $ V.RowType V.U
      pure (RecordType r, V.U)
    R.RecordCon rts -> do
      rts' <- labelsUnique ctx rts
      tvas <- traverse goInfer rts'
      pure (RecordCon $ fst <$> tvas, V.RecordType $ V.RowCon $ snd <$> tvas)
    R.RecordProj label rt -> do
      (t, vr0) <- goInfer rt
      forceValue vr0 >>= \case
        V.RecordType (V.RowCon as)
          | Just a <- as ^. at label -> pure (RecordProj label t, a)
        vr -> throw $ ElabError ctx (FieldExpected label vr)

labelsUnique ::
  Eff (Throw ElabError) m => Context -> [(Name, a)] -> m (HashMap Name a)
labelsUnique ctx = foldlM go mempty
 where
  go m (x, t)
    | Just _ <- m ^. at x = throw $ ElabError ctx (DupField x)
    | otherwise = pure $ m & at x ?!~ t
