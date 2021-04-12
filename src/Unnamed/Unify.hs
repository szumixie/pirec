module Unnamed.Unify (unify) where

import Relude

import Control.Effect
import Control.Effect.Error
import Optics

import Unnamed.Data.MultiMap qualified as MM
import Unnamed.Data.MultiMapAlter qualified as MMA
import Unnamed.Env qualified as Env
import Unnamed.Syntax.Core
import Unnamed.Value (Spine, Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Meta (Meta)

import Unnamed.Effect.Meta
import Unnamed.Eval
import Unnamed.Unify.Error

declareFieldLabels
  [d|
    data Renaming = Renaming
      { source :: {-# UNPACK #-} Level
      , map :: HashMap Level Level
      }
      deriving stock (Show)
    |]

liftRenaming :: Level -> Renaming -> Renaming
liftRenaming tgt (Renaming src m) =
  Renaming (src + 1) (m & at tgt ?!~ src)

invert :: Effs [MetaLookup, Throw UnifyError] m => Spine -> m Renaming
invert = go
 where
  go = \case
    V.Nil -> pure $ Renaming 0 mempty
    V.App spine t -> do
      Renaming src m <- go spine
      forceValue t >>= \case
        V.Neut (V.Rigid lx) V.Nil
          | Just _ <- m ^. at lx -> throw $ Nonlinear lx
          | otherwise -> pure $ Renaming (src + 1) (m & at lx ?!~ src)
        t -> throw $ Nonvariable t
    _ -> throw NonInvertable

rename ::
  Effs [MetaLookup, Throw UnifyError] m =>
  Meta ->
  Renaming ->
  Level ->
  Value ->
  m Term
rename meta = quoteWith go liftRenaming
 where
  go (Renaming _ m) = \case
    V.Rigid lx -> case m ^. at lx of
      Nothing -> throw $ ScopeError lx
      Just lx -> pure $ Var lx
    V.Flex mx
      | mx == meta -> throw $ OccursError meta
      | otherwise -> pure $ Meta mx Nothing

solve ::
  Effs [MetaCtx, Throw UnifyError] m => Level -> Meta -> Spine -> Value -> m ()
solve lvl meta spine t = do
  renaming <- invert spine
  t <- rename meta renaming lvl t
  solution <-
    eval Env.empty . flipfoldl' ($) t $
      replicate (renaming ^. #source % coerced) (Lam "x")
  solveMeta meta solution

unify :: Effs [MetaCtx, Throw UnifyError] m => Level -> Value -> Value -> m ()
unify lvl = go
 where
  go =
    curry $
      traverseOf both forceValue >=> \case
        (V.Neut x spine, V.Neut x' spine') | x == x' -> goSpine spine spine'
         where
          goSpine = curry \case
            (V.Nil, V.Nil) -> pass
            (V.App spine t, V.App spine' t') ->
              goSpine spine spine' *> go t t'
            (V.RowExt ts spine, V.RowExt ts' spine')
              | Just ms <- MM.match go ts ts' -> do
                sequenceA_ ms
                goSpine spine spine'
            ( V.RecordProj label index spine
              , V.RecordProj label' index' spine'
              )
                | label == label' && index == index' ->
                  goSpine spine spine'
            (V.RecordAlter ts spine, V.RecordAlter ts' spine')
              | Just ms <- MMA.match go ts ts' -> do
                sequenceA_ ms
                goSpine spine spine'
            (spine, spine') ->
              throw $ Mismatch (V.Neut x spine) (V.Neut x' spine')
        (V.Neut x (V.RowExt ts spine), V.Neut x' (V.RowExt ts' spine'))
          | Just ms <- MM.match go ts ts' -> do
            sequenceA_ ms
            go (V.Neut x spine) (V.Neut x' spine')
        ( V.Neut (V.Flex mx) (V.RowExt ts spine)
          , V.Neut x' (V.RowExt ts' spine')
          )
            | Just ts' <- MM.superDifference ts' ts ->
              solve lvl mx spine $ V.Neut x' (V.RowExt ts' spine')
        ( V.Neut x (V.RowExt ts spine)
          , V.Neut (V.Flex mx') (V.RowExt ts' spine')
          )
            | Just ts <- MM.superDifference ts ts' ->
              solve lvl mx' spine' $ V.Neut x (V.RowExt ts spine)
        ( V.Neut (V.Flex mx) (V.RowExt ts spine)
          , V.Neut (V.Flex mx') (V.RowExt ts' spine')
          ) -> do
            mnew <- freshMeta
            solve lvl mx spine $
              V.Neut (V.Flex mnew) (V.RowExt (MM.difference ts' ts) spine')
            solve lvl mx' spine' $
              V.Neut (V.Flex mnew) (V.RowExt (MM.difference ts ts') spine)
        (V.Neut (V.Flex mx) (V.RowExt ts spine), V.RowLit ts') ->
          solve lvl mx spine $ V.RowLit (MM.difference ts' ts)
        (V.RowLit ts, V.Neut (V.Flex mx) (V.RowExt ts' spine)) ->
          solve lvl mx spine $ V.RowLit (MM.difference ts ts')
        (V.Neut (V.Flex mx) spine, t) -> solve lvl mx spine t
        (t, V.Neut (V.Flex mx) spine) -> solve lvl mx spine t
        (V.U, V.U) -> pass
        (V.Pi _ a closure, V.Pi _ a' closure') -> do
          go a a'
          b <- openClosure lvl closure
          b' <- openClosure lvl closure'
          unify (lvl + 1) b b'
        (V.Lam _ closure, V.Lam _ closure') -> do
          t <- openClosure lvl closure
          t' <- openClosure lvl closure'
          unify (lvl + 1) t t'
        (V.Lam _ closure, V.Neut x spine) -> do
          t <- openClosure lvl closure
          unify (lvl + 1) t (V.Neut x $ V.App spine (V.var lvl))
        (V.Neut x spine, V.Lam _ closure) -> do
          t <- openClosure lvl closure
          unify (lvl + 1) (V.Neut x $ V.App spine (V.var lvl)) t
        (V.RowType a, V.RowType a') -> go a a'
        (V.RowLit ts, V.RowLit ts')
          | Just ms <- MM.match go ts ts' ->
            sequenceA_ ms
        (V.RecordType va, V.RecordType va') -> go va va'
        (V.RecordLit ts, V.RecordLit ts')
          | Just ms <- MM.match go ts ts' ->
            sequenceA_ ms
        (V.RecordLit ts, t'@V.Neut{}) ->
          ifor_ ts \(label, index) t -> go t (V.recordProj label index t')
        (t@V.Neut{}, V.RecordLit ts') ->
          ifor_ ts' \(label, index) t' -> go (V.recordProj label index t') t
        (t, t') -> throw $ Mismatch t t'
