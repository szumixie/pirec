module Unnamed.Elab (check, infer) where

import Relude
import Relude.Extra.Tuple (traverseToFst)

import Control.Effect
import Control.Effect.Error
import Optics

import Unnamed.Data.MultiMapAlter qualified as MMA
import Unnamed.Plicity
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
  pure $ Meta meta (Just $ Ctx.boundMask ctx)

insertMetaValue :: Eff MetaCtx m => Context -> m Value
insertMetaValue ctx = eval (Ctx.env ctx) =<< insertMeta ctx

elabUnify ::
  Effs [MetaCtx, Throw ElabError] m => Context -> Value -> Value -> m ()
elabUnify ctx t t' =
  throwToThrow (ElabError ctx . UnifyError t t') $
    unify (Ctx.level ctx) t t'

check ::
  Effs [MetaCtx, Throw ElabError] m => Context -> R.Term -> Value -> m Term
check !ctx = curry \case
  (R.Span span t, a) -> check (ctx & Ctx.span .~ span) t a
  (R.Hole, _) -> insertMeta ctx
  (R.Let x a t u, vb) -> do
    (t, va) <- optionalCheck ctx t a
    vt <- eval (Ctx.env ctx) t
    u <- check (ctx & Ctx.extend x va vt) u vb
    pure $ Let x t u
  (R.Lam pl x a t, V.Pi pl' _ va' closure) | pl == pl' -> do
    whenJust a \a -> do
      va <- eval (Ctx.env ctx) =<< check ctx a V.Univ
      elabUnify ctx va va'
    vb <- openClosure (Ctx.level ctx) closure
    Lam pl x <$> check (ctx & Ctx.bind x va') t vb
  (t, V.Pi Implicit x _ closure) -> do
    vb <- openClosure (Ctx.level ctx) closure
    Lam Implicit x <$> check (ctx & Ctx.insert x) t vb
  (t, va') -> do
    (t, va) <- uncurry (insertImplAppNoBeta ctx) =<< infer ctx t
    t <$ elabUnify ctx va va'

infer ::
  Effs [MetaCtx, Throw ElabError] m => Context -> R.Term -> m (Term, Value)
infer !ctx = goInfer
 where
  goCheck = check ctx
  goInfer = \case
    R.Span span t -> infer (ctx & Ctx.span .~ span) t
    R.Var x -> case ctx & Ctx.getName x of
      Nothing -> throw $ ElabError ctx (ScopeError x)
      Just (lx, va) -> pure (Var lx, va)
    R.Hole -> (,) <$> insertMeta ctx <*> insertMetaValue ctx
    R.Let x a t u -> do
      (t, va) <- optionalCheck ctx t a
      vt <- eval (Ctx.env ctx) t
      (u, vb) <- infer (ctx & Ctx.extend x va vt) u
      pure (Let x t u, vb)
    R.Univ -> pure (Univ, V.Univ)
    R.Pi pl x a b -> do
      a <- case a of
        Nothing -> insertMeta ctx
        Just a -> goCheck a V.Univ
      va <- eval (Ctx.env ctx) a
      b <- check (ctx & Ctx.bind x va) b V.Univ
      pure (Pi pl x a b, V.Univ)
    R.Lam pl x a t -> do
      va <- case a of
        Nothing -> insertMetaValue ctx
        Just a -> eval (Ctx.env ctx) =<< goCheck a V.Univ
      (t, vb) <-
        uncurry (insertImplAppNoBeta ctx) =<< infer (ctx & Ctx.bind x va) t
      closure <- closeValue (Ctx.env ctx) vb
      pure (Lam pl x t, V.Pi pl x va closure)
    R.App pl t u -> do
      (t, vp) <- case pl of
        Explicit -> uncurry (insertImplApp ctx) =<< goInfer t
        Implicit -> goInfer t
      (va, closure) <-
        forceValue vp >>= \case
          V.Pi pl' _ va closure
            | pl' == pl -> pure (va, closure)
            | otherwise -> throw $ ElabError ctx (PlicityMismatch pl pl')
          vp -> do
            va <- insertMetaValue ctx
            closure <-
              V.Closure (Ctx.env ctx) <$> insertMeta (ctx & Ctx.bind "v" va)
            elabUnify ctx vp $ V.Pi pl "v" va closure
            pure (va, closure)
      u <- goCheck u va
      vu <- eval (Ctx.env ctx) u
      vb <- appClosure closure vu
      pure (App pl t u, vb)
    R.RowType a -> do
      a <- goCheck a V.Univ
      pure (RowType a, V.Univ)
    R.RowEmpty -> do
      va <- insertMetaValue ctx
      pure (RowLit mempty, V.RowType va)
    R.RowExt label t ts -> do
      (t, va) <- goInfer t
      ts <- goCheck ts (V.RowType va)
      pure (RowExt (one (label, t)) ts, V.RowType va)
    R.RecordType r -> do
      r <- goCheck r (V.RowType V.Univ)
      pure (RecordType r, V.Univ)
    R.RecordEmpty -> do
      pure (RecordLit mempty, V.RecordType $ V.RowLit mempty)
    R.RecordExt label a t u -> do
      (t, va) <- optionalCheck ctx t a
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

optionalCheck ::
  Effs [MetaCtx, Throw ElabError] m =>
  Context ->
  R.Term ->
  Maybe R.Term ->
  m (Term, Value)
optionalCheck ctx t = \case
  Nothing -> infer ctx t
  Just a -> do
    va <- eval (Ctx.env ctx) =<< check ctx a V.Univ
    traverseToFst (check ctx t) va

insertImplApp ::
  Effs [MetaCtx, Throw ElabError] m =>
  Context ->
  Term ->
  Value ->
  m (Term, Value)
insertImplApp ctx = go
 where
  go t =
    forceValue >=> \case
      V.Pi Implicit _ _ closure -> do
        u <- insertMeta ctx
        go (App Implicit t u) =<< appClosure closure =<< eval (Ctx.env ctx) u
      va -> pure (t, va)

insertImplAppNoBeta ::
  Effs [MetaCtx, Throw ElabError] m =>
  Context ->
  Term ->
  Value ->
  m (Term, Value)
insertImplAppNoBeta ctx t va = case t of
  Lam Implicit _ _ -> pure (t, va)
  _ -> insertImplApp ctx t va
