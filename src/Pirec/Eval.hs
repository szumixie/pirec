module Pirec.Eval (
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

import Pirec.BoundMask qualified as BM
import Pirec.Env (Env)
import Pirec.Env qualified as Env
import Pirec.Plicity (Plicity (..))
import Pirec.Syntax.Core (Term (..))
import Pirec.Value (Closure, Spine, Value)
import Pirec.Value qualified as V
import Pirec.Var.Level (Level)
import Pirec.Var.Meta (Meta)

import Pirec.Effect.Meta

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
              env
                & foldrOf
                  (BM.masked mask)
                  (flip $ appValuePure mlookup Explicit)
                  t
           where
            t = mlookup mx ?: V.meta mx
          Let _ t u -> goEnv (env & Env.extend (go t)) u
          Univ -> V.Univ
          Pi pl x a b -> V.Pi pl x (go a) (V.Closure env b)
          Lam pl x t -> V.Lam pl x (V.Closure env t)
          App pl t u -> appValuePure mlookup pl (go t) (go u)
          RowType a -> V.RowType (go a)
          RowLit ts -> V.RowLit (go <$> ts)
          RowExt ts row -> V.rowExt (go <$> ts) (go row)
          RecordType row -> V.RecordType (go row)
          RecordLit ts -> V.RecordLit (go <$> ts)
          RecordProj lbl index t -> V.recordProj lbl index (go t)
          RecordAlter ts u -> V.recordAlter (go <$> ts) (go u)
  pure $ goEnv env t

appClosure :: Eff MetaLookup m => Closure -> Value -> m Value
appClosure (V.Closure env t) u = eval (env & Env.extend u) t

appValue :: Eff MetaLookup m => Plicity -> Value -> Value -> m Value
appValue pl t u = case t of
  V.Neut x spine -> pure $ V.Neut x (V.App pl spine u)
  V.Lam pl' _ closure | pl == pl' -> appClosure closure u
  _ -> error "bug"

appValuePure :: (Meta -> Maybe Value) -> Plicity -> Value -> Value -> Value
appValuePure mlookup pl t u = run $ runMetaLookup mlookup (appValue pl t u)

appSpine :: Eff MetaLookup m => Value -> Spine -> m Value
appSpine t = go
 where
  go = \case
    V.Nil -> pure t
    V.App pl spine u -> do
      spine <- go spine
      appValue pl spine u
    V.RowExt us spine -> V.rowExt us <$> go spine
    V.RecordProj lbl index spine -> V.recordProj lbl index <$> go spine
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
                V.App pl spine t -> App pl <$> goSpine spine <*> go t
                V.RowExt ts spine -> RowExt <$> traverse go ts <*> goSpine spine
                V.RecordProj lbl index spine ->
                  RecordProj lbl index <$> goSpine spine
                V.RecordAlter ts spine ->
                  RecordAlter <$> traverse go ts <*> goSpine spine
          goSpine spine
        V.Univ -> pure Univ
        V.Pi pl x a closure -> Pi pl x <$> go a <*> goClosure closure
        V.Lam pl x closure -> Lam pl x <$> goClosure closure
        V.RowType a -> RowType <$> go a
        V.RowLit ts -> RowLit <$> traverse go ts
        V.RecordType row -> RecordType <$> go row
        V.RecordLit ts -> RecordLit <$> traverse go ts
    goClosure closure = goAcc (f lvl acc) (lvl + 1) =<< openClosure lvl closure
{-# INLINE quoteWith #-}

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
