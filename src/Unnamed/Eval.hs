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
import Unnamed.Var.Name (Name)

import Unnamed.Effect.Meta

eval :: Eff MetaLookup m => Env Value -> Term -> m Value
eval env0 t0 = do
  mget <- metaLookup
  let goEnv !env = go
       where
        go = \case
          Var lx -> env & Env.index lx ?: error "bug"
          Meta mx Nothing -> mget mx ?: V.meta mx
          Meta mx (Just mask) ->
            env
              & foldlOf'
                (BM.masked mask)
                (appValuePure mget)
                (mget mx ?: V.meta mx)
          Let _ _ t u -> goEnv (env & Env.extend (go t)) u
          U -> V.U
          Pi x a b -> V.Pi x (go a) $ V.Closure env b
          Lam x t -> V.Lam x $ V.Closure env t
          App t u -> appValuePure mget (go t) (go u)
          RowType labels a -> V.RowType labels $ go a
          RowLit ts -> V.RowLit $ go <$> ts
          RowCons ts r -> rowConsValue (go <$> ts) (go r)
          RecordType r -> V.RecordType $ go r
          RecordLit ts -> V.RecordLit $ go <$> ts
          RecordProj label t -> recordProjValue label $ go t
  pure $ goEnv env0 t0

appClosure :: Eff MetaLookup m => Closure -> Value -> m Value
appClosure (V.Closure env t) vu = eval (env & Env.extend vu) t

appValue :: Eff MetaLookup m => Value -> Value -> m Value
appValue vt vu = case vt of
  V.Neut x spine -> pure $ V.Neut x (V.App spine vu)
  V.Lam _ closure -> appClosure closure vu
  _ -> error "bug"

appValuePure :: (Meta -> Maybe Value) -> Value -> Value -> Value
appValuePure mget vt vu = run $ runMetaLookup mget $ appValue vt vu

rowConsValue :: HashMap Name Value -> Value -> Value
rowConsValue vts = \case
  V.Neut x spine -> V.Neut x $ V.rowCons vts spine
  V.RowLit vus -> V.RowLit $ vts <> vus
  _ -> error "bug"

recordProjValue :: Name -> Value -> Value
recordProjValue label = \case
  V.Neut x spine -> V.Neut x $ V.RecordProj label spine
  V.RecordLit vts -> vts ^. at label ?: error "bug"
  _ -> error "bug"

appSpine :: Eff MetaLookup m => Value -> Spine -> m Value
appSpine vt = go
 where
  go = \case
    V.Nil -> pure vt
    V.App spine vu -> go spine >>= \vt' -> appValue vt' vu
    V.RowCons vts spine -> rowConsValue vts <$> go spine
    V.RecordProj label spine -> recordProjValue label <$> go spine

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
              V.App spine vt -> App <$> goSpine spine <*> go vt
              V.RowCons vts spine ->
                RowCons <$> traverse go vts <*> goSpine spine
              V.RecordProj label spine -> RecordProj label <$> goSpine spine
         in goSpine spine0
      V.U -> pure U
      V.Pi x va closure ->
        Pi x <$> go va <*> (quote (lvl + 1) =<< openClosure lvl closure)
      V.Lam x closure ->
        Lam x <$> (quote (lvl + 1) =<< openClosure lvl closure)
      V.RowType labels va -> RowType labels <$> go va
      V.RowLit ts -> RowLit <$> traverse go ts
      V.RecordType vr -> RecordType <$> go vr
      V.RecordLit vts -> RecordLit <$> traverse go vts

openClosure :: Eff MetaLookup m => Level -> Closure -> m Value
openClosure lvl closure = appClosure closure (V.var lvl)

closeValue :: Eff MetaLookup m => Env Value -> Value -> m Closure
closeValue env = fmap (V.Closure env) . quote (Env.level env + 1)

normal :: Eff MetaLookup m => Term -> m Term
normal = eval Env.empty >=> quote 0
