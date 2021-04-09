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
      , target :: {-# UNPACK #-} Level
      , map :: HashMap Level Level
      }
      deriving stock (Show)
    |]

liftRenaming :: Renaming -> Renaming
liftRenaming (Renaming src tgt m) =
  Renaming (src + 1) (tgt + 1) (m & at tgt ?!~ src)

invert :: Effs [MetaLookup, Throw UnifyError] m => Level -> Spine -> m Renaming
invert lvl = go >>> fmap \(src, m) -> Renaming src lvl m
 where
  go = \case
    V.Nil -> pure (0, mempty)
    V.App spine vt -> do
      (src, m) <- go spine
      forceValue vt >>= \case
        V.Neut (V.Rigid lx) V.Nil
          | Just _ <- m ^. at lx -> throw $ Nonlinear lx
          | otherwise -> pure (src + 1, m & at lx ?!~ src)
        vt' -> throw $ Nonvariable vt'
    _ -> throw NonInvertable

rename ::
  Effs [MetaLookup, Throw UnifyError] m => Meta -> Renaming -> Value -> m Term
rename meta = goRenaming
 where
  goRenaming renaming@(Renaming _ tgt m) = go
   where
    go =
      forceValue >=> \case
        V.Neut x spine0 ->
          let goSpine = \case
                V.Nil -> case x of
                  V.Rigid lx -> case m ^. at lx of
                    Nothing -> throw $ ScopeError lx
                    Just lx' -> pure $ Var lx'
                  V.Flex mx
                    | mx == meta -> throw $ OccursError meta
                    | otherwise -> pure $ Meta mx Nothing
                V.App spine vt ->
                  App <$> goSpine spine <*> go vt
                V.RowExt vts spine ->
                  RowExt <$> traverse go vts <*> goSpine spine
                V.RecordProj label index spine ->
                  RecordProj label index <$> goSpine spine
                V.RecordAlter vts spine ->
                  RecordAlter <$> traverse go vts <*> goSpine spine
           in goSpine spine0
        V.U -> pure U
        V.Pi x va closure -> Pi x <$> go va <*> goClosure closure
        V.Lam x closure -> Lam x <$> goClosure closure
        V.RowType va -> RowType <$> go va
        V.RowLit vts -> RowLit <$> traverse go vts
        V.RecordType vr -> RecordType <$> go vr
        V.RecordLit vts -> RecordLit <$> traverse go vts
    goClosure closure =
      goRenaming (liftRenaming renaming) =<< openClosure tgt closure

solve ::
  Effs [MetaCtx, Throw UnifyError] m => Level -> Meta -> Spine -> Value -> m ()
solve lvl meta spine vt = do
  renaming <- invert lvl spine
  t <- rename meta renaming vt
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
        (V.Neut x spine0, V.Neut x' spine0')
          | x == x' ->
            let goSpine = curry \case
                  (V.Nil, V.Nil) -> pass
                  (V.App spine vt, V.App spine' vt') ->
                    goSpine spine spine' *> go vt vt'
                  (V.RowExt vts spine, V.RowExt vts' spine')
                    | Just ms <- MM.match go vts vts' -> do
                      sequenceA_ ms
                      goSpine spine spine'
                  ( V.RecordProj label index spine
                    , V.RecordProj label' index' spine'
                    )
                      | label == label' && index == index' ->
                        goSpine spine spine'
                  (V.RecordAlter vts spine, V.RecordAlter vts' spine')
                    | Just ms <- MMA.match go vts vts' -> do
                      sequenceA_ ms
                      goSpine spine spine'
                  (spine, spine') ->
                    throw $ Mismatch (V.Neut x spine) (V.Neut x' spine')
             in goSpine spine0 spine0'
        (V.Neut x (V.RowExt vts spine), V.Neut x' (V.RowExt vts' spine'))
          | Just ms <- MM.match go vts vts' -> do
            sequenceA_ ms
            go (V.Neut x spine) (V.Neut x' spine')
        ( V.Neut (V.Flex mx) (V.RowExt vts spine)
          , V.Neut x' (V.RowExt vts' spine')
          )
            | Just vts'' <- MM.superDifference vts' vts ->
              solve lvl mx spine $ V.Neut x' (V.RowExt vts'' spine')
        ( V.Neut x (V.RowExt vts spine)
          , V.Neut (V.Flex mx') (V.RowExt vts' spine')
          )
            | Just vts'' <- MM.superDifference vts vts' ->
              solve lvl mx' spine' $
                V.Neut x (V.RowExt vts'' spine)
        ( V.Neut (V.Flex mx) (V.RowExt vts spine)
          , V.Neut (V.Flex mx') (V.RowExt vts' spine')
          ) -> do
            mx'' <- freshMeta
            solve lvl mx spine $
              V.Neut (V.Flex mx'') (V.RowExt (MM.difference vts' vts) spine')
            solve lvl mx' spine' $
              V.Neut (V.Flex mx'') (V.RowExt (MM.difference vts vts') spine)
        (V.Neut (V.Flex mx) (V.RowExt vts spine), V.RowLit vts') ->
          solve lvl mx spine $ V.RowLit (MM.difference vts' vts)
        (V.RowLit vts, V.Neut (V.Flex mx) (V.RowExt vts' spine)) ->
          solve lvl mx spine $ V.RowLit (MM.difference vts vts')
        (V.Neut (V.Flex mx) spine, vt) -> solve lvl mx spine vt
        (vt, V.Neut (V.Flex mx) spine) -> solve lvl mx spine vt
        (V.U, V.U) -> pass
        (V.Pi _ va closure, V.Pi _ va' closure') -> do
          go va va'
          vb <- openClosure lvl closure
          vb' <- openClosure lvl closure'
          unify (lvl + 1) vb vb'
        (V.Lam _ closure, V.Lam _ closure') -> do
          vt <- openClosure lvl closure
          vt' <- openClosure lvl closure'
          unify (lvl + 1) vt vt'
        (V.Lam _ closure, V.Neut x spine) -> do
          vt <- openClosure lvl closure
          unify (lvl + 1) vt (V.Neut x $ V.App spine (V.var lvl))
        (V.Neut x spine, V.Lam _ closure) -> do
          vt <- openClosure lvl closure
          unify (lvl + 1) (V.Neut x $ V.App spine (V.var lvl)) vt
        (V.RowType va, V.RowType va') -> go va va'
        (V.RowLit vts, V.RowLit vts')
          | Just ms <- MM.match go vts vts' ->
            sequenceA_ ms
        (V.RecordType va, V.RecordType va') -> go va va'
        (V.RecordLit vts, V.RecordLit vts')
          | Just ms <- MM.match go vts vts' ->
            sequenceA_ ms
        (V.RecordLit vts, vt'@V.Neut{}) ->
          ifor_ vts \(label, index) vt -> go vt (V.recordProj label index vt')
        (vt@V.Neut{}, V.RecordLit vts') ->
          ifor_ vts' \(label, index) vt' -> go (V.recordProj label index vt') vt
        (vt, vt') -> throw $ Mismatch vt vt'
