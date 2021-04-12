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
elabUnify ctx t t' =
  throwToThrow (ElabError ctx . UnifyError t t') $
    unify (ctx ^. #level) t t'

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
checkInfer !ctx = (goCheck, goInfer)
 where
  goCheck = curry \case
    (R.Span span t, a) -> check (ctx & #span .~ span) t a
    (R.Hole, _) -> insertMeta ctx
    (R.Let x a t u, vb) -> do
      (t, va) <- case a of
        Nothing -> goInfer t
        Just a -> do
          va <- eval (ctx ^. #env) =<< goCheck a V.U
          traverseToFst (goCheck t) va
      vt <- eval (ctx ^. #env) t
      u <- check (ctx & Ctx.extend x va vt) u vb
      pure $ Let x t u
    (R.Lam x a t, V.Pi _ va' closure) -> do
      whenJust a \a -> do
        va <- eval (ctx ^. #env) =<< goCheck a V.U
        elabUnify ctx va va'
      vb <- openClosure (ctx ^. #level) closure
      Lam x <$> check (ctx & Ctx.bind x va') t vb
    (t, va') -> do
      (t, va) <- goInfer t
      t <$ elabUnify ctx va va'

  goInfer = \case
    R.Span span t -> infer (ctx & #span .~ span) t
    R.Var x -> case ctx ^. #names % at x of
      Nothing -> throw $ ElabError ctx (ScopeError x)
      Just (lx, va) -> pure (Var lx, va)
    R.Hole -> (,) <$> insertMeta ctx <*> insertMetaValue ctx
    R.Let x a t u -> do
      (t, va) <- case a of
        Nothing -> goInfer t
        Just a -> do
          va <- eval (ctx ^. #env) =<< goCheck a V.U
          traverseToFst (goCheck t) va
      vt <- eval (ctx ^. #env) t
      (u, vb) <- infer (ctx & Ctx.extend x va vt) u
      pure (Let x t u, vb)
    R.U -> pure (U, V.U)
    R.Pi x a b -> do
      a <- goCheck a V.U
      va <- eval (ctx ^. #env) a
      b <- check (ctx & Ctx.bind x va) b V.U
      pure (Pi x a b, V.U)
    R.Lam x a t -> do
      va <- case a of
        Nothing -> insertMetaValue ctx
        Just a -> eval (ctx ^. #env) =<< goCheck a V.U
      (t, vb) <- infer (ctx & Ctx.bind x va) t
      closure <- closeValue (ctx ^. #env) vb
      pure (Lam x t, V.Pi x va closure)
    R.App t u -> do
      (t, vp) <- goInfer t
      (va, closure) <-
        forceValue vp >>= \case
          V.Pi _ va closure -> pure (va, closure)
          vp -> do
            va <- insertMetaValue ctx
            closure <-
              V.Closure (ctx ^. #env) <$> insertMeta (ctx & Ctx.bind "x" va)
            elabUnify ctx vp $ V.Pi "x" va closure
            pure (va, closure)
      u <- goCheck u va
      vu <- eval (ctx ^. #env) u
      vb <- appClosure closure vu
      pure (App t u, vb)
    R.RowType a -> do
      a <- goCheck a V.U
      pure (RowType a, V.U)
    R.RowEmpty -> do
      va <- insertMetaValue ctx
      pure (RowLit mempty, V.RowType va)
    R.RowExt label t ts -> do
      (t, va) <- goInfer t
      ts <- goCheck ts (V.RowType va)
      pure (RowExt (one (label, t)) ts, V.RowType va)
    R.RecordType r -> do
      r <- goCheck r (V.RowType V.U)
      pure (RecordType r, V.U)
    R.RecordEmpty -> do
      pure (RecordLit mempty, V.RecordType $ V.RowLit mempty)
    R.RecordExt label t u -> do
      (t, va) <- goInfer t
      (u, vp) <- goInfer u
      vr <- insertMetaValue ctx
      elabUnify ctx vp $ V.RecordType vr
      pure
        ( RecordAlter (MMA.singleInsert label t) u
        , V.RecordType (V.rowExt (one (label, va)) vr)
        )
    R.RecordProj label t -> do
      (t, vp) <- goInfer t
      va <-
        forceValue vp >>= \case
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
    R.RecordRestr label t -> do
      (t, vp) <- goInfer t
      va <- insertMetaValue ctx
      vr <- insertMetaValue ctx
      elabUnify ctx vp $ V.RecordType (V.rowExt (one (label, va)) vr)
      pure (RecordAlter (MMA.singleDelete label) t, V.RecordType vr)
