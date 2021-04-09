module Unnamed.Eval (
  eval,
  appClosure,
  forceValue,
  quote,
  openClosure,
  closeValue,
  normal,
) where

import Relude

import Control.Effect
import Optics

import Unnamed.BoundMask qualified as BM
import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Value (Closure, Spine, Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Meta (Meta)

import Unnamed.Effect.Meta

eval :: Eff MetaLookup m => Env Value -> Term -> m Value
eval env0 t0 = do
  mlookup <- metaLookup
  let goEnv !env = go
       where
        go = \case
          Var lx -> env & Env.index lx ?: error "bug"
          Meta mx mmask ->
            let vt = mlookup mx ?: V.meta mx
             in case mmask of
                  Nothing -> vt
                  Just mask ->
                    env & foldlOf' (BM.masked mask) (appValuePure mlookup) vt
          Let _ t u -> goEnv (env & Env.extend (go t)) u
          U -> V.U
          Pi x a b -> V.Pi x (go a) (V.Closure env b)
          Lam x t -> V.Lam x (V.Closure env t)
          App t u -> appValuePure mlookup (go t) (go u)
          RowType a -> V.RowType (go a)
          RowLit ts -> V.RowLit (go <$> ts)
          RowExt ts row -> V.rowExt (go <$> ts) (go row)
          RecordType row -> V.RecordType (go row)
          RecordLit ts -> V.RecordLit (go <$> ts)
          RecordProj label index t -> V.recordProj label index (go t)
          RecordAlter ts u -> V.recordAlter (go <$> ts) (go u)
  pure $ goEnv env0 t0

appClosure :: Eff MetaLookup m => Closure -> Value -> m Value
appClosure (V.Closure env t) vu = eval (env & Env.extend vu) t

appValue :: Eff MetaLookup m => Value -> Value -> m Value
appValue vt vu = case vt of
  V.Neut x spine -> pure $ V.Neut x (V.App spine vu)
  V.Lam _ closure -> appClosure closure vu
  _ -> error "bug"

appValuePure :: (Meta -> Maybe Value) -> Value -> Value -> Value
appValuePure mlookup vt vu = run $ runMetaLookup mlookup (appValue vt vu)

appSpine :: Eff MetaLookup m => Value -> Spine -> m Value
appSpine vt = go
 where
  go = \case
    V.Nil -> pure vt
    V.App spine vu -> go spine >>= (`appValue` vu)
    V.RowExt vus spine -> V.rowExt vus <$> go spine
    V.RecordProj label index spine -> V.recordProj label index <$> go spine
    V.RecordAlter vus spine -> V.recordAlter vus <$> go spine

forceValue :: Eff MetaLookup m => Value -> m Value
forceValue vt0 = case vt0 of
  V.Neut (V.Flex mx) spine ->
    metaLookup ?? mx >>= \case
      Just vt -> appSpine vt spine >>= forceValue
      Nothing -> pure vt0
  vt -> pure vt

quote :: Eff MetaLookup m => Level -> Value -> m Term
quote !lvl = go
 where
  go =
    forceValue >=> \case
      V.Neut x spine0 ->
        let goSpine = \case
              V.Nil -> pure case x of
                V.Rigid lx -> Var lx
                V.Flex mx -> Meta mx Nothing
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
      V.Pi x a closure -> Pi x <$> go a <*> quoteClosure lvl closure
      V.Lam x closure -> Lam x <$> quoteClosure lvl closure
      V.RowType a -> RowType <$> go a
      V.RowLit ts -> RowLit <$> traverse go ts
      V.RecordType row -> RecordType <$> go row
      V.RecordLit ts -> RecordLit <$> traverse go ts

quoteClosure :: Eff MetaLookup m => Level -> Closure -> m Term
quoteClosure lvl = quote (lvl + 1) <=< openClosure lvl

openClosure :: Eff MetaLookup m => Level -> Closure -> m Value
openClosure lvl closure = appClosure closure (V.var lvl)

closeValue :: Eff MetaLookup m => Env Value -> Value -> m Closure
closeValue env = fmap (V.Closure env) . quote (Env.level env + 1)

normal :: Eff MetaLookup m => Term -> m Term
normal = quote 0 <=< eval Env.empty
