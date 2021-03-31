module Unnamed.Elab (check, infer) where

import Relude

import Data.HashMap.Lazy qualified as Map
import Data.HashSet qualified as Set

import Control.Effect
import Control.Effect.Error
import Optics

import Unnamed.LabelSet qualified as LS
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
    R.Hole -> (,) <$> insertMeta ctx <*> insertMetaValue ctx
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
      va <- insertMetaValue ctx
      (t, vb) <- infer (ctx & Ctx.bind x va) rt
      closure <- closeValue (ctx ^. #env) vb
      pure (Lam x t, V.Pi x va closure)
    R.App rt ru -> do
      (t, vp0) <- goInfer rt
      (va, closure) <-
        forceValue vp0 >>= \case
          V.Pi _ va vb -> pure (va, vb)
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
    R.RowType labels ra -> do
      a <- goCheck ra V.U
      ulabels <- labelsUnique ctx $ (,()) <$> labels
      pure (RowType (LS.Lacks $ Set.fromMap ulabels) a, V.U)
    R.RowLit [] -> do
      va <- insertMetaValue ctx
      pure (RowLit mempty, V.RowType mempty va)
    R.RowLit ((label1, rt1) : rts) -> do
      (t1, va) <- goInfer rt1
      rts' <- labelsUnique ctx ((label1, rt1) : rts)
      ts <- ifor rts' \label rt ->
        if label == label1 then pure t1 else goCheck rt va
      pure (RowLit ts, V.RowType (LS.Has $ Map.keysSet ts) va)
    R.RowCons rts rr -> do
      (r, vrt0) <- goInfer rr
      rts' <- labelsUnique ctx rts
      let labelSet = Map.keysSet rts'
      forceValue vrt0 >>= \case
        V.RowType labels va -> do
          unless (LS.disjoint (LS.Has labelSet) labels) $
            case LS.intersection (LS.Has labelSet) labels of
              LS.Has ilabels -> throw $ ElabError ctx (FieldOverlap ilabels)
              LS.Lacks _ -> error "bug"
          ts <- traverse (`goCheck` va) rts'
          pure (RowCons ts r, V.RowType (LS.Has labelSet <> labels) va)
        vrt -> do
          va <- insertMetaValue ctx
          elabUnify ctx vrt $ V.RowType (LS.Lacks labelSet) va
          ts <- traverse (`goCheck` va) rts'
          pure (RowCons ts r, V.RowType LS.full va)
    R.RecordType rr -> do
      r <- goCheck rr $ V.RowType LS.full V.U
      pure (RecordType r, V.U)
    R.RecordLit rts -> do
      rts' <- labelsUnique ctx rts
      tvas <- traverse goInfer rts'
      pure (RecordLit $ fst <$> tvas, V.RecordType $ V.RowLit (snd <$> tvas))
    R.RecordProj label rt -> do
      (t, vr0) <- goInfer rt
      forceValue vr0 >>= \case
        V.RecordType (V.RowLit as)
          | Just a <- as ^. at label -> pure (RecordProj label t, a)
        vr -> do
          va <- insertMetaValue ctx
          vr' <- insertMetaValue ctx
          elabUnify ctx vr $ V.RecordType (rowConsValue (one (label, va)) vr')
          pure (RecordProj label t, va)
    R.RecordMod label ru rt -> do
      (t, vr0) <- goInfer rt
      (u, vb) <- goInfer ru
      forceValue vr0 >>= \case
        V.RecordType (V.RowLit as) ->
          pure
            ( RecordMod label u t
            , V.RecordType $ V.RowLit (as & at label ?~ vb)
            )
        vr -> do
          va <- insertMetaValue ctx
          vr' <- insertMetaValue ctx
          elabUnify ctx vr $ V.RecordType (rowConsValue (one (label, va)) vr')
          pure
            ( RecordMod label u t
            , V.RecordType $ rowConsValue (one (label, vb)) vr'
            )

labelsUnique ::
  Eff (Throw ElabError) m => Context -> [(Name, a)] -> m (HashMap Name a)
labelsUnique ctx = foldlM go mempty
 where
  go m (x, t)
    | Just _ <- m ^. at x = throw $ ElabError ctx (DupField x)
    | otherwise = pure $ m & at x ?!~ t
