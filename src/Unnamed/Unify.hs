module Unnamed.Unify (unify) where

import Relude

import Data.HashMap.Lazy qualified as Map

import Control.Effect
import Control.Effect.Error
import Optics

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
    V.RecordProj{} -> throw ProjError

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
                V.App spine vu -> App <$> goSpine spine <*> go vu
                V.RecordProj label spine -> RecordProj label <$> goSpine spine
           in goSpine spine0
        V.U -> pure U
        V.Pi x va closure ->
          Pi x <$> go va
            <*> (goRenaming (liftRenaming renaming) =<< openClosure tgt closure)
        V.Lam x closure ->
          Lam x
            <$> (goRenaming (liftRenaming renaming) =<< openClosure tgt closure)
        V.RowType va -> RowType <$> go va
        V.RowCon ts -> RowCon <$> traverse go ts
        V.RecordType vr -> RecordType <$> go vr
        V.RecordCon ts -> RecordCon <$> traverse go ts

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
                  (V.RecordProj label spine, V.RecordProj label' spine')
                    | label == label' -> goSpine spine spine'
                  (spine, spine') ->
                    throw $ Mismatch (V.Neut x spine) (V.Neut x spine')
             in goSpine spine0 spine0'
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
        (V.RowCon vts, V.RowCon vts')
          | Map.keysSet vts == Map.keysSet vts' ->
            sequenceA_ $ Map.intersectionWith go vts vts'
        (V.RecordType va, V.RecordType va') -> go va va'
        (V.RecordCon vts, V.RecordCon vts')
          | Map.keysSet vts == Map.keysSet vts' ->
            sequenceA_ $ Map.intersectionWith go vts vts'
        (V.RecordCon vts, V.Neut x spine) ->
          ifor_ vts \label vt -> go vt (V.Neut x $ V.RecordProj label spine)
        (V.Neut x spine, V.RecordCon vts) ->
          ifor_ vts \label vt -> go (V.Neut x $ V.RecordProj label spine) vt
        (vt, vt') -> throw $ Mismatch vt vt'
