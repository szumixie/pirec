module Unnamed.Elab (check, infer) where

import Control.Monad (foldM)

import Data.HashMap.Strict qualified as Map

import Optics

import Unnamed.Elab.Context (Context (..))
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
    (V.Row a, V.Row a') -> go a a'
    (V.RowCon ts, V.RowCon ts') ->
      Map.keysSet ts == Map.keysSet ts' && and (Map.intersectionWith go ts ts')
    (V.Record r, V.Record r') -> go r r'
    (V.RecordCon ts, V.RecordCon ts') ->
      Map.keysSet ts == Map.keysSet ts' && and (Map.intersectionWith go ts ts')
    (V.RecordCon ts, V.Neut t') ->
      ts & iallOf ifolded \x t -> go t (V.Neut $ V.RecordProj x t')
    (V.Neut t, V.RecordCon ts') ->
      ts' & iallOf ifolded \x t' -> go t' (V.Neut $ V.RecordProj x t)
    _ -> False

  goNeut = curry \case
    (V.Var x, V.Var x') -> x == x'
    (V.App t u, V.App t' u') -> goNeut t t' && go u u'
    (V.RecordProj f t, V.RecordProj f' t') -> f == f' && goNeut t t'
    _ -> False

type ElabM = Either ElabError

check :: Context -> R.Term -> Value -> ElabM Term
check = fst . checkInfer

infer :: Context -> R.Term -> ElabM (Term, Value)
infer = snd . checkInfer

checkInfer ::
  Context -> (R.Term -> Value -> ElabM Term, R.Term -> ElabM (Term, Value))
checkInfer ctx@(Context lvl env names) = (goCheck, goInfer)
 where
  goCheck = withPos \pos -> curry \case
    (R.Let x a t u, b) -> do
      a' <- goCheck a V.U
      let va = eval env a'
      t' <- goCheck t va
      u' <- check (ctx & Ctx.extend x va (eval env t')) u b
      pure $ Let x a' t' u'
    (R.Lam x t, V.Pi _ a b) ->
      Lam x <$> check (Ctx.bind x a ctx) t (openClosure lvl b)
    (R.RowCon ts, V.Row a) -> do
      ts' <- labelsUnique pos ts
      RowCon <$> traverse (`goCheck` a) ts'
    (R.RecordCon ts, V.Record (V.RowCon as)) -> do
      ts' <- labelsUnique pos ts
      let tset = Map.keysSet ts'
          aset = Map.keysSet as
      if tset /= aset
        then Left $ ElabError pos ctx (FieldMismatch tset aset)
        else
          RecordCon <$> ifor ts' \x t -> case as ^. at x of
            Nothing -> error "bug"
            Just a -> goCheck t a
    (t, a) -> do
      (t', a') <- goInfer $ WithPos pos t
      if conv lvl a a'
        then pure t'
        else Left $ ElabError pos ctx (ConvError a a')

  goInfer = withPos \pos -> \case
    R.Var x -> case names ^. at x of
      Nothing -> Left $ ElabError pos ctx (ScopeError x)
      Just (l, t) -> Right (Var l, t)
    R.Let x a t u -> do
      a' <- goCheck a V.U
      let va = eval env a'
      t' <- goCheck t va
      (u', b) <- infer (ctx & Ctx.extend x va (eval env t')) u
      pure (Let x a' t' u', b)
    R.U -> pure (U, V.U)
    R.Pi x a b -> do
      a' <- goCheck a V.U
      b' <- check (ctx & Ctx.bind x (eval env a')) b V.U
      pure (Pi x a' b', V.U)
    R.Lam{} -> Left $ ElabError pos ctx LamInference
    R.App t u -> do
      (t', p) <- goInfer t
      case p of
        V.Pi _ a b -> do
          u' <- goCheck u a
          pure (App t' u', appClosure b (eval env u'))
        _ -> Left $ ElabError pos ctx (PiExpected p)
    R.Row a -> do
      a' <- goCheck a V.U
      pure (Row a', V.U)
    R.RowCon [] -> Left $ ElabError pos ctx EmptyRowInference
    R.RowCon ((x0, t0) : ts) -> do
      (t0', a) <- goInfer t0
      ts' <- labelsUnique pos ((x0, t0) : ts)
      tsc <- ifor ts' \x t -> if x == x0 then pure t0' else goCheck t a
      pure (RowCon tsc, V.Row a)
    R.Record r -> do
      r' <- goCheck r $ V.Row V.U
      pure (Record r', V.U)
    R.RecordCon ts -> do
      ts' <- labelsUnique pos ts
      tsc <- traverse goInfer ts'
      pure (RecordCon $ fst <$> tsc, V.RowCon $ snd <$> tsc)
    R.RecordProj f t -> do
      (t', r) <- goInfer t
      case r of
        V.Record (V.RowCon as)
          | Just a <- as ^. at f -> pure (RecordProj f t', a)
        _ -> Left $ ElabError pos ctx (FieldExpected f r)

  labelsUnique pos = foldM go Map.empty
   where
    go m (x, t)
      | x `Map.member` m = Left $ ElabError pos ctx (DupField x)
      | otherwise = pure $ m & at x ?!~ t
