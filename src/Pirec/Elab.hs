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
import Pirec.Value (Value)
import Pirec.Value qualified as V

import Pirec.Effect.Meta
import Pirec.Elab.Context (Context)
import Pirec.Elab.Context qualified as Ctx
import Pirec.Elab.Error
import Pirec.Eval
import Pirec.Unify (unify)

insertMeta :: Effs '[MetaState, Ask Context] m => m Term
insertMeta = Meta <$> freshMeta <*> (Just <$> asks Ctx.boundMask)

elabEval :: Effs '[MetaLookup, Ask Context] m => Term -> m Value
elabEval t = asks Ctx.env >>= \env -> eval env t

elabThrow :: Effs [Throw ElabError, Ask Context] m => ElabErrorType -> m a
elabThrow err = ask >>= \ctx -> throw $ ElabError ctx err

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
      vt <- elabEval t
      u <- local (Ctx.extend x va vt) (goCheck u vb)
      pure $ Let x t u
    (R.Lam pl x a t, V.Pi pl' _ va' closure) | pl == pl' -> do
      whenJust a \a -> do
        va <- elabEval =<< goCheck a V.Univ
        elabUnify va va'
      vb <- asks Ctx.level >>= \lvl -> openClosure lvl closure
      Lam pl x <$> local (Ctx.bind x va') (goCheck t vb)
    (t, V.Pi Implicit x _ closure) -> do
      vb <- asks Ctx.level >>= \lvl -> openClosure lvl closure
      Lam Implicit x <$> local (Ctx.insert x) (goCheck t vb)
    (t, va') -> do
      (t, va) <- uncurry insertImplAppNoBeta =<< goInfer t
      t <$ elabUnify va va'

  goInfer = \case
    R.Span span t -> local (Ctx.span .~ span) (goInfer t)
    R.Var x ->
      asks (Ctx.getName x) >>= \case
        Nothing -> elabThrow $ ScopeError x
        Just (lx, va) -> pure (Var lx, va)
    R.Hole -> (,) <$> insertMeta <*> (elabEval =<< insertMeta)
    R.Let x a t u -> do
      (t, va) <- optionalCheck t a
      vt <- elabEval t
      (u, vb) <- local (Ctx.extend x va vt) (goInfer u)
      pure (Let x t u, vb)
    R.Univ -> pure (Univ, V.Univ)
    R.Pi pl x a b -> do
      a <- optionalType a
      va <- elabEval a
      b <- local (Ctx.bind x va) (goCheck b V.Univ)
      pure (Pi pl x a b, V.Univ)
    R.Lam pl x a t -> do
      va <- elabEval =<< optionalType a
      (t, vb) <- uncurry insertImplAppNoBeta =<< local (Ctx.bind x va) (goInfer t)
      closure <- asks Ctx.env >>= \env -> closeValue env vb
      pure (Lam pl x t, V.Pi pl x va closure)
    R.App pl t u -> do
      (t, vp) <- case pl of
        Explicit -> uncurry insertImplApp =<< goInfer t
        Implicit -> goInfer t
      (va, closure) <-
        forceValue vp >>= \case
          V.Pi pl' _ va closure
            | pl' == pl -> pure (va, closure)
            | otherwise -> elabThrow $ PlicityMismatch pl pl'
          vp -> do
            va <- elabEval =<< insertMeta
            closure <-
              asks Ctx.env >>= \env ->
                V.Closure env <$> local (Ctx.bind "v" va) insertMeta
            elabUnify vp (V.Pi pl "v" va closure)
            pure (va, closure)
      u <- goCheck u va
      vu <- elabEval u
      vb <- appClosure closure vu
      pure (App pl t u, vb)
    R.RowType a -> do
      a <- goCheck a V.Univ
      pure (RowType a, V.Univ)
    R.RowEmpty -> do
      va <- elabEval =<< insertMeta
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
      (t, va) <- optionalCheck t a
      (u, vp) <- goInfer u
      vr <- elabEval =<< insertMeta
      elabUnify vp (V.RecordType vr)
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
            va <- elabEval =<< insertMeta
            vr <- elabEval =<< insertMeta
            elabUnify vp $ V.RecordType (V.rowExt (one (label, va)) vr)
            pure va
      pure (RecordProj label 0 t, va)
    R.RecordRestr label t -> do
      (t, vp) <- goInfer t
      va <- elabEval =<< insertMeta
      vr <- elabEval =<< insertMeta
      elabUnify vp $ V.RecordType (V.rowExt (one (label, va)) vr)
      pure (RecordAlter (MMA.singleDelete label) t, V.RecordType vr)

  optionalCheck t = \case
    Nothing -> goInfer t
    Just a -> do
      va <- elabEval =<< goCheck a V.Univ
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
        go (App Implicit t u) =<< appClosure closure =<< elabEval u
      va -> pure (t, va)

insertImplAppNoBeta ::
  Effs [MetaCtx, Throw ElabError, Ask Context] m =>
  Term ->
  Value ->
  m (Term, Value)
insertImplAppNoBeta t va = case t of
  Lam Implicit _ _ -> pure (t, va)
  _ -> insertImplApp t va
