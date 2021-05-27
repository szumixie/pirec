module Pirec.Elab (check, infer) where

import Relude hiding (Reader, ask, asks, local)
import Relude.Extra.Tuple (traverseToFst)

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
import Optics

import Pirec.Data.MultiMapAlter qualified as MMA
import Pirec.Plicity (Plicity (..))
import Pirec.Syntax.Core (Term (..))
import Pirec.Syntax.Raw qualified as R
import Pirec.Value (Closure, Value)
import Pirec.Value qualified as V

import Pirec.Effect.Meta
import Pirec.Elab.Context (Context)
import Pirec.Elab.Context qualified as Ctx
import Pirec.Elab.Error
import Pirec.Eval
import Pirec.Unify (unify)

insertMeta :: Effs [MetaState, Ask Context] m => m Term
insertMeta = Meta <$> freshMeta <*> (Just <$> asks Ctx.boundMask)

evalCtx :: Effs [MetaLookup, Ask Context] m => Term -> m Value
evalCtx t = asks Ctx.env >>= \env -> eval env t

closureCtx :: Eff (Ask Context) m => Term -> m Closure
closureCtx t = asks Ctx.env <&> \env -> V.Closure env t

openClosureCtx :: Effs [MetaLookup, Ask Context] m => Closure -> m Value
openClosureCtx closure = asks Ctx.level >>= \lvl -> openClosure lvl closure

closeValueCtx :: Effs [MetaLookup, Ask Context] m => Value -> m Closure
closeValueCtx t = asks Ctx.env >>= \env -> closeValue env t

throwCtx :: Effs [Throw ElabError, Ask Context] m => ElabErrorType -> m a
throwCtx err = ask >>= \ctx -> throw $ ElabError ctx err

elabUnify ::
  Effs [MetaCtx, Throw ElabError, Ask Context] m => Value -> Value -> m ()
elabUnify t t' = do
  ctx <- ask
  throwToThrow (ElabError ctx . UnifyError t t') $
    unify (Ctx.level ctx) t t'

check ::
  Effs [MetaCtx, Throw ElabError, Reader Context] m => R.Term -> Value -> m Term
check = fst checkInfer

infer ::
  Effs [MetaCtx, Throw ElabError, Reader Context] m => R.Term -> m (Term, Value)
infer = snd checkInfer

checkInfer ::
  Effs [MetaCtx, Throw ElabError, Reader Context] m =>
  (R.Term -> Value -> m Term, R.Term -> m (Term, Value))
checkInfer = (goCheck, goInfer)
 where
  goCheck = curry \case
    (R.Span span t, va) -> local (Ctx.span .~ span) (goCheck t va)
    (R.Hole, _) -> insertMeta
    (R.Let x a t u, vb) -> do
      (t, va) <- optionalCheck t a
      vt <- evalCtx t
      u <- local (Ctx.extend x va vt) (goCheck u vb)
      pure $ Let x t u
    (R.Lam pl x a t, V.Pi pl' _ va' closure) | pl == pl' -> do
      whenJust a \a -> do
        va <- evalCtx =<< goCheck a V.Univ
        elabUnify va va'
      vb <- openClosureCtx closure
      Lam pl x <$> local (Ctx.bind x va') (goCheck t vb)
    (t, V.Pi Implicit x _ closure) -> do
      vb <- openClosureCtx closure
      Lam Implicit x <$> local (Ctx.insert x) (goCheck t vb)
    (R.Pair t u, V.Sigma _ va closure) -> do
      t <- goCheck t va
      u <- goCheck u =<< appClosure closure =<< evalCtx t
      pure $ Pair t u
    (t, va) -> do
      (t, va') <- uncurry insertImplAppNoBeta =<< goInfer t
      t <$ elabUnify va va'

  goInfer = \case
    R.Span span t -> local (Ctx.span .~ span) (goInfer t)
    R.Var x ->
      asks (Ctx.getName x) >>= \case
        Nothing -> throwCtx $ ScopeError x
        Just (lx, va) -> pure (Var lx, va)
    R.Hole -> (,) <$> insertMeta <*> (evalCtx =<< insertMeta)
    R.Let x a t u -> do
      (t, va) <- optionalCheck t a
      vt <- evalCtx t
      (u, vb) <- local (Ctx.extend x va vt) (goInfer u)
      pure (Let x t u, vb)
    R.Univ -> pure (Univ, V.Univ)
    R.Pi pl x a b -> do
      a <- optionalType a
      va <- evalCtx a
      b <- local (Ctx.bind x va) (goCheck b V.Univ)
      pure (Pi pl x a b, V.Univ)
    R.Lam pl x a t -> do
      va <- evalCtx =<< optionalType a
      (t, vb) <-
        uncurry insertImplAppNoBeta =<< local (Ctx.bind x va) (goInfer t)
      closure <- closeValueCtx vb
      pure (Lam pl x t, V.Pi pl x va closure)
    R.App pl t u -> do
      (t, vp) <- case pl of
        Explicit -> uncurry insertImplApp =<< goInfer t
        Implicit -> goInfer t
      (va, closure) <-
        forceValue vp >>= \case
          V.Pi pl' _ va closure
            | pl' == pl -> pure (va, closure)
            | otherwise -> throwCtx $ PlicityMismatch pl pl'
          vp -> do
            va <- evalCtx =<< insertMeta
            closure <- closureCtx =<< local (Ctx.bind "v" va) insertMeta
            elabUnify vp (V.Pi pl "v" va closure)
            pure (va, closure)
      u <- goCheck u va
      vu <- evalCtx u
      vb <- appClosure closure vu
      pure (App pl t u, vb)
    R.Sigma x a b -> do
      a <- optionalType a
      va <- evalCtx a
      b <- local (Ctx.bind x va) (goCheck b V.Univ)
      pure (Sigma x a b, V.Univ)
    R.Pair t u -> do
      (t, va) <- goInfer t
      (u, vb) <- goInfer u
      closure <- closeValueCtx vb
      pure (Pair t u, V.Sigma "_" va closure)
    R.Proj1 t -> do
      (t, vp) <- goInfer t
      va <-
        forceValue vp >>= \case
          V.Sigma _ va _ -> pure va
          vp -> do
            va <- evalCtx =<< insertMeta
            closure <- closureCtx =<< local (Ctx.bind "v" va) insertMeta
            elabUnify vp (V.Sigma "v" va closure)
            pure va
      pure (Proj1 t, va)
    R.Proj2 t -> do
      (t, vp) <- goInfer t
      closure <-
        forceValue vp >>= \case
          V.Sigma _ _ closure -> pure closure
          vp -> do
            va <- evalCtx =<< insertMeta
            closure <- closureCtx =<< local (Ctx.bind "v" va) insertMeta
            elabUnify vp (V.Sigma "v" va closure)
            pure closure
      vb <- appClosure closure . V.proj1 =<< evalCtx t
      pure (Proj2 t, vb)
    R.RowType a -> do
      a <- goCheck a V.Univ
      pure (RowType a, V.Univ)
    R.RowEmpty -> do
      va <- evalCtx =<< insertMeta
      pure (RowLit mempty, V.RowType va)
    R.RowExt lbl t ts -> do
      (t, va) <- goInfer t
      ts <- goCheck ts (V.RowType va)
      pure (RowExt (one (lbl, t)) ts, V.RowType va)
    R.RecordType r -> do
      r <- goCheck r (V.RowType V.Univ)
      pure (RecordType r, V.Univ)
    R.RecordEmpty -> do
      pure (RecordLit mempty, V.RecordType $ V.RowLit mempty)
    R.RecordExt lbl a t u -> do
      (t, va) <- optionalCheck t a
      (u, vp) <- goInfer u
      vr <- evalCtx =<< insertMeta
      elabUnify vp (V.RecordType vr)
      pure
        ( RecordAlter (MMA.singleInsert lbl t) u
        , V.RecordType (V.rowExt (one (lbl, va)) vr)
        )
    R.RecordProj lbl t -> do
      (t, vp) <- goInfer t
      va <-
        forceValue vp >>= \case
          V.RecordType (V.RowLit vas)
            | Just va <- vas ^? ix (lbl, 0) -> pure va
          V.RecordType (V.Neut _ (V.RowExt vas _))
            | Just va <- vas ^? ix (lbl, 0) -> pure va
          vp -> do
            va <- evalCtx =<< insertMeta
            vr <- evalCtx =<< insertMeta
            elabUnify vp $ V.RecordType (V.rowExt (one (lbl, va)) vr)
            pure va
      pure (RecordProj lbl 0 t, va)
    R.RecordRestr lbl t -> do
      (t, vp) <- goInfer t
      va <- evalCtx =<< insertMeta
      vr <- evalCtx =<< insertMeta
      elabUnify vp $ V.RecordType (V.rowExt (one (lbl, va)) vr)
      pure (RecordAlter (MMA.singleDelete lbl) t, V.RecordType vr)

  optionalCheck t = \case
    Nothing -> goInfer t
    Just a -> do
      va <- evalCtx =<< goCheck a V.Univ
      traverseToFst (goCheck t) va

  optionalType = \case
    Nothing -> insertMeta
    Just a -> goCheck a V.Univ

insertImplApp ::
  Effs [MetaCtx, Throw ElabError, Ask Context] m =>
  Term ->
  Value ->
  m (Term, Value)
insertImplApp = go
 where
  go t =
    forceValue >=> \case
      V.Pi Implicit _ _ closure -> do
        u <- insertMeta
        go (App Implicit t u) =<< appClosure closure =<< evalCtx u
      va -> pure (t, va)

insertImplAppNoBeta ::
  Effs [MetaCtx, Throw ElabError, Ask Context] m =>
  Term ->
  Value ->
  m (Term, Value)
insertImplAppNoBeta t va = case t of
  Lam Implicit _ _ -> pure (t, va)
  _ -> insertImplApp t va
