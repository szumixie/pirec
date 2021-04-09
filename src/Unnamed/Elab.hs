module Unnamed.Elab (check, infer) where

import Relude
import Relude.Extra.Tuple (traverseToFst)

import Control.Effect
import Control.Effect.Error
import Optics

import Unnamed.Data.MultiMapAlter qualified as MMA
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Syntax.Raw qualified as R
import Unnamed.Value (Value)
import Unnamed.Value qualified as V

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

insertMetaValue :: Eff MetaCtx m => Context -> m Value
insertMetaValue ctx = eval (ctx ^. #env) =<< insertMeta ctx

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
    (R.Span sp rt, va) -> check (ctx & #span .~ sp) rt va
    (R.Hole, _) -> insertMeta ctx
    (R.Let x mra rt ru, vb) -> do
      (t, va) <- case mra of
        Nothing -> goInfer rt
        Just ra -> do
          va <- eval (ctx ^. #env) =<< goCheck ra V.U
          traverseToFst (goCheck rt) va
      vt <- eval (ctx ^. #env) t
      u <- check (ctx & Ctx.extend x va vt) ru vb
      pure $ Let x t u
    (R.Lam x mra rt, V.Pi _ va' closure) -> do
      whenJust mra \ra -> do
        va <- eval (ctx ^. #env) =<< goCheck ra V.U
        elabUnify ctx va va'
      vb <- openClosure (ctx ^. #level) closure
      Lam x <$> check (ctx & Ctx.bind x va') rt vb
    (rt, va') -> do
      (t, va) <- goInfer rt
      t <$ elabUnify ctx va va'

  goInfer = \case
    R.Span sp rt -> infer (ctx & #span .~ sp) rt
    R.Var x -> case ctx ^. #names % at x of
      Nothing -> throw $ ElabError ctx (ScopeError x)
      Just (lx, va) -> pure (Var lx, va)
    R.Hole -> (,) <$> insertMeta ctx <*> insertMetaValue ctx
    R.Let x mra rt ru -> do
      (t, va) <- case mra of
        Nothing -> goInfer rt
        Just ra -> do
          va <- eval (ctx ^. #env) =<< goCheck ra V.U
          traverseToFst (goCheck rt) va
      vt <- eval (ctx ^. #env) t
      (u, vb) <- infer (ctx & Ctx.extend x va vt) ru
      pure (Let x t u, vb)
    R.U -> pure (U, V.U)
    R.Pi x ra rb -> do
      a <- goCheck ra V.U
      va <- eval (ctx ^. #env) a
      b <- check (ctx & Ctx.bind x va) rb V.U
      pure (Pi x a b, V.U)
    R.Lam x mra rt -> do
      va <- case mra of
        Nothing -> insertMetaValue ctx
        Just ra -> eval (ctx ^. #env) =<< goCheck ra V.U
      (t, vb) <- infer (ctx & Ctx.bind x va) rt
      closure <- closeValue (ctx ^. #env) vb
      pure (Lam x t, V.Pi x va closure)
    R.App rt ru -> do
      (t, vp0) <- goInfer rt
      (va, closure) <-
        forceValue vp0 >>= \case
          V.Pi _ va closure -> pure (va, closure)
          vp -> do
            va <- insertMetaValue ctx
            closure <-
              V.Closure (ctx ^. #env) <$> insertMeta (ctx & Ctx.bind "x" va)
            elabUnify ctx vp $ V.Pi "x" va closure
            pure (va, closure)
      u <- goCheck ru va
      vu <- eval (ctx ^. #env) u
      vb <- appClosure closure vu
      pure (App t u, vb)
    R.RowType ra -> do
      a <- goCheck ra V.U
      pure (RowType a, V.U)
    R.RowEmpty -> do
      va <- insertMetaValue ctx
      pure (RowLit mempty, V.RowType va)
    R.RowExt label rt rts -> do
      (t, va) <- goInfer rt
      ts <- goCheck rts (V.RowType va)
      pure (RowExt (one (label, t)) ts, V.RowType va)
    R.RecordType rr -> do
      r <- goCheck rr (V.RowType V.U)
      pure (RecordType r, V.U)
    R.RecordEmpty -> do
      pure (RecordLit mempty, V.RecordType $ V.RowLit mempty)
    R.RecordExt label rt ru -> do
      (t, va) <- goInfer rt
      (u, vp) <- goInfer ru
      vr <- insertMetaValue ctx
      elabUnify ctx vp $ V.RecordType vr
      pure
        ( RecordAlter (MMA.singleInsert label t) u
        , V.RecordType (V.rowExt (one (label, va)) vr)
        )
    R.RecordProj label rt -> do
      (t, vp0) <- goInfer rt
      va <-
        forceValue vp0 >>= \case
          V.RecordType (V.RowLit vas)
            | Just va <- vas ^? ix (label, 0) -> pure va
          V.RecordType (V.Neut _ (V.RowExt vas _))
            | Just va <- vas ^? ix (label, 0) -> pure va
          vp -> do
            va <- insertMetaValue ctx
            vr <- insertMetaValue ctx
            elabUnify ctx vp $ V.RecordType (V.rowExt (one (label, va)) vr)
            pure va
      pure (RecordProj label 0 t, va)
    R.RecordRestr label rt -> do
      (t, vp) <- goInfer rt
      va <- insertMetaValue ctx
      vr <- insertMetaValue ctx
      elabUnify ctx vp $ V.RecordType (V.rowExt (one (label, va)) vr)
      pure (RecordAlter (MMA.singleDelete label) t, V.RecordType vr)
