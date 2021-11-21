module Pirec.Unify (unify) where

import Relude

import Control.Effect
import Control.Effect.Error
import Optics

import Pirec.Data.MultiMap qualified as MM
import Pirec.Data.MultiMapAlter qualified as MMA
import Pirec.Env qualified as Env
import Pirec.Syntax.Core
import Pirec.Value (Spine, Value)
import Pirec.Value qualified as V
import Pirec.Var.Level (Level (..))
import Pirec.Var.Meta (Meta)

import Pirec.Effect.Meta
import Pirec.Eval
import Pirec.Unify.Error
import Pirec.Unify.Renaming (Renaming)
import Pirec.Unify.Renaming qualified as Renaming

rename ::
  Effs [MetaLookup, Throw UnifyError] m =>
  Meta ->
  Renaming ->
  Level ->
  Value ->
  m Term
rename meta = quoteWith go Renaming.lift
 where
  go renaming = \case
    V.Rigid lx -> case renaming & Renaming.rename lx of
      Nothing -> throw $ ScopeError meta lx
      Just lx -> pure $ Var lx
    V.Flex mx
      | mx == meta -> throw $ OccursError meta
      | otherwise -> pure $ Meta mx Nothing

abstract :: Spine -> Term -> Term
abstract spine t = case spine of
  V.Nil -> t
  V.App pl spine _ -> abstract spine $ Lam pl "v" t
  _ -> error "bug"

solve ::
  Effs [MetaCtx, Throw UnifyError] m => Level -> Meta -> Spine -> Value -> m ()
solve lvl meta spine t = do
  renaming <- Renaming.invert spine
  t <- rename meta renaming lvl t
  solution <- eval Env.empty $ abstract spine t
  solveMeta meta solution

unify :: Effs [MetaCtx, Throw UnifyError] m => Level -> Value -> Value -> m ()
unify !lvl = go
 where
  go =
    curry $
      traverseOf both forceValue >=> \case
        (V.Neut x spine, V.Neut x' spine') | x == x' -> goSpine spine spine'
         where
          goSpine = curry \case
            (V.Nil, V.Nil) -> pass
            (V.App pl spine t, V.App pl' spine' t')
              | pl == pl' -> goSpine spine spine' *> go t t'
            (V.Proj1 spine, V.Proj1 spine') -> goSpine spine spine'
            (V.Proj2 spine, V.Proj2 spine') -> goSpine spine spine'
            (V.RowExt ts spine, V.RowExt ts' spine')
              | Just ms <- MM.matchWith go ts ts' ->
                sequenceA_ ms *> goSpine spine spine'
            (V.RecordProj lbl index spine, V.RecordProj lbl' index' spine')
              | lbl == lbl' && index == index' -> goSpine spine spine'
            (V.RecordAlter ts spine, V.RecordAlter ts' spine')
              | Just ms <- MMA.matchWith go ts ts' ->
                sequenceA_ ms *> goSpine spine spine'
            (spine, spine') ->
              throw $ Mismatch (V.Neut x spine) (V.Neut x' spine')
        (V.Neut x (V.RowExt ts spine), V.Neut x' (V.RowExt ts' spine'))
          | Just ms <- MM.matchWith go ts ts' ->
            sequenceA_ ms *> go (V.Neut x spine) (V.Neut x' spine')
        ( V.Neut (V.Flex mx) (V.RowExt ts spine)
          , V.Neut x' (V.RowExt ts' spine')
          )
            | ts `MM.isSubsetOf` ts' -> do
              solve lvl mx spine $
                V.Neut x' (V.RowExt (MM.difference ts' ts) spine')
              sequenceA_ $ MM.intersectionWith go ts ts'
        ( V.Neut x (V.RowExt ts spine)
          , V.Neut (V.Flex mx') (V.RowExt ts' spine')
          )
            | ts' `MM.isSubsetOf` ts -> do
              solve lvl mx' spine' $
                V.Neut x (V.RowExt (MM.difference ts ts') spine)
              sequenceA_ $ MM.intersectionWith go ts ts'
        ( V.Neut (V.Flex mx) (V.RowExt ts spine)
          , V.Neut (V.Flex mx') (V.RowExt ts' spine')
          ) -> do
            mnew <- freshMeta
            solve lvl mx spine $
              V.Neut (V.Flex mnew) (V.RowExt (MM.difference ts' ts) spine')
            solve lvl mx' spine' $
              V.Neut (V.Flex mnew) (V.RowExt (MM.difference ts ts') spine)
            sequenceA_ $ MM.intersectionWith go ts ts'
        (V.Neut (V.Flex mx) (V.RowExt ts spine), V.RowLit ts') -> do
          solve lvl mx spine $ V.RowLit (MM.difference ts' ts)
          sequenceA_ $ MM.intersectionWith go ts ts'
        (V.RowLit ts, V.Neut (V.Flex mx) (V.RowExt ts' spine)) -> do
          solve lvl mx spine $ V.RowLit (MM.difference ts ts')
          sequenceA_ $ MM.intersectionWith go ts ts'
        (V.Neut (V.Flex mx) spine, t) -> solve lvl mx spine t
        (t, V.Neut (V.Flex mx) spine) -> solve lvl mx spine t
        (V.Univ, V.Univ) -> pass
        (V.Pi pl _ a closure, V.Pi pl' _ a' closure') | pl == pl' -> do
          go a a'
          b <- openClosure lvl closure
          b' <- openClosure lvl closure'
          unify (lvl + 1) b b'
        (V.Lam pl _ closure, V.Lam pl' _ closure') | pl == pl' -> do
          t <- openClosure lvl closure
          t' <- openClosure lvl closure'
          unify (lvl + 1) t t'
        (V.Lam pl _ closure, V.Neut x spine) -> do
          t <- openClosure lvl closure
          unify (lvl + 1) t (V.Neut x $ V.App pl spine (V.var lvl))
        (V.Neut x spine, V.Lam pl _ closure) -> do
          t <- openClosure lvl closure
          unify (lvl + 1) (V.Neut x $ V.App pl spine (V.var lvl)) t
        (V.Sigma _ a closure, V.Sigma _ a' closure') -> do
          go a a'
          b <- openClosure lvl closure
          b' <- openClosure lvl closure'
          unify (lvl + 1) b b'
        (V.Pair t u, V.Pair t' u') -> go t t' *> go u u'
        (V.Pair t u, V.Neut x spine) ->
          go t (V.Neut x $ V.Proj1 spine) *> go u (V.Neut x $ V.Proj2 spine)
        (V.Neut x spine, V.Pair t u) ->
          go (V.Neut x $ V.Proj1 spine) t *> go (V.Neut x $ V.Proj2 spine) u
        (V.RowType a, V.RowType a') -> go a a'
        (V.RowLit ts, V.RowLit ts')
          | Just ms <- MM.matchWith go ts ts' -> sequenceA_ ms
        (V.RecordType va, V.RecordType va') -> go va va'
        (V.RecordLit ts, V.RecordLit ts')
          | Just ms <- MM.matchWith go ts ts' -> sequenceA_ ms
        (V.RecordLit ts, t'@V.Neut{}) ->
          ifor_ ts \(lbl, index) t -> go t (V.recordProj lbl index t')
        (t@V.Neut{}, V.RecordLit ts') ->
          ifor_ ts' \(lbl, index) t' -> go (V.recordProj lbl index t) t'
        (t, t') -> throw $ Mismatch t t'
