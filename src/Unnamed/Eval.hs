module Unnamed.Eval (
  eval,
  appClosure,
  forceValue,
  quoteWith,
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
eval env t = do
  mlookup <- metaLookup
  let goEnv !env = go
       where
        go = \case
          Var lx -> env & Env.index lx ?: error "bug"
          Meta mx mmask -> case mmask of
            Nothing -> t
            Just mask ->
              env & foldrOf (BM.masked mask) (flip $ appValuePure mlookup) t
           where
            t = mlookup mx ?: V.meta mx
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
  pure $ goEnv env t

appClosure :: Eff MetaLookup m => Closure -> Value -> m Value
appClosure (V.Closure env t) u = eval (env & Env.extend u) t

appValue :: Eff MetaLookup m => Value -> Value -> m Value
appValue t u = case t of
  V.Neut x spine -> pure $ V.Neut x (V.App spine u)
  V.Lam _ closure -> appClosure closure u
  _ -> error "bug"

appValuePure :: (Meta -> Maybe Value) -> Value -> Value -> Value
appValuePure mlookup t u = run $ runMetaLookup mlookup (appValue t u)

appSpine :: Eff MetaLookup m => Value -> Spine -> m Value
appSpine t = go
 where
  go = \case
    V.Nil -> pure t
    V.App spine u -> go spine >>= (`appValue` u)
    V.RowExt us spine -> V.rowExt us <$> go spine
    V.RecordProj label index spine -> V.recordProj label index <$> go spine
    V.RecordAlter us spine -> V.recordAlter us <$> go spine

forceValue :: Eff MetaLookup m => Value -> m Value
forceValue t = case t of
  V.Neut (V.Flex mx) spine ->
    metaLookup ?? mx >>= \case
      Just t -> appSpine t spine >>= forceValue
      Nothing -> pure t
  _ -> pure t

quoteWith ::
  Eff MetaLookup m =>
  (a -> V.Var -> m Term) ->
  (Level -> a -> a) ->
  a ->
  Level ->
  Value ->
  m Term
quoteWith quoteVar f = goAcc
 where
  goAcc !acc !lvl = go
   where
    go =
      forceValue >=> \case
        V.Neut x spine -> do
          x <- quoteVar acc x
          let goSpine = \case
                V.Nil -> pure x
                V.App spine t -> App <$> goSpine spine <*> go t
                V.RowExt ts spine -> RowExt <$> traverse go ts <*> goSpine spine
                V.RecordProj label index spine ->
                  RecordProj label index <$> goSpine spine
                V.RecordAlter ts spine ->
                  RecordAlter <$> traverse go ts <*> goSpine spine
          goSpine spine
        V.U -> pure U
        V.Pi x a closure -> Pi x <$> go a <*> goClosure closure
        V.Lam x closure -> Lam x <$> goClosure closure
        V.RowType a -> RowType <$> go a
        V.RowLit ts -> RowLit <$> traverse go ts
        V.RecordType row -> RecordType <$> go row
        V.RecordLit ts -> RecordLit <$> traverse go ts
    goClosure closure = goAcc (f lvl acc) (lvl + 1) =<< openClosure lvl closure

quote :: Eff MetaLookup m => Level -> Value -> m Term
quote = quoteWith go (\_ _ -> ()) ()
 where
  go _ x = pure case x of
    V.Rigid lx -> Var lx
    V.Flex mx -> Meta mx Nothing

openClosure :: Eff MetaLookup m => Level -> Closure -> m Value
openClosure lvl closure = appClosure closure (V.var lvl)

closeValue :: Eff MetaLookup m => Env Value -> Value -> m Closure
closeValue env = fmap (V.Closure env) . quote (Env.level env + 1)

normal :: Eff MetaLookup m => Term -> m Term
normal = quote 0 <=< eval Env.empty
