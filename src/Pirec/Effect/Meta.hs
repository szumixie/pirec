module Pirec.Effect.Meta (
  MetaLookup (..),
  MetaState (..),
  MetaCtx,
  metaLookup,
  freshMeta,
  solveMeta,
  runMetaLookup,
  metaCtxToIO,
  MetaCtxToIOC,
  MetaLookupC,
  MetaStateC,
) where

import Relude

import Data.HashTable.IO (BasicHashTable)
import Data.HashTable.IO qualified as HT
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed.Mutable qualified as U
import System.IO.Unsafe (unsafeDupablePerformIO)

import Control.Effect

import Pirec.Value (Value)
import Pirec.Var.Meta (Meta (..))

data MetaLookup :: Effect where
  MetaLookup :: MetaLookup m (Meta -> Maybe Value)

metaLookup :: Eff MetaLookup m => m (Meta -> Maybe Value)
metaLookup = send MetaLookup
{-# INLINE metaLookup #-}

data MetaState :: Effect where
  FreshMeta :: MetaState m Meta
  SolveMeta :: Meta -> Value -> MetaState m ()

freshMeta :: Eff MetaState m => m Meta
freshMeta = send FreshMeta
{-# INLINE freshMeta #-}

solveMeta :: Eff MetaState m => Meta -> Value -> m ()
solveMeta m vt = send $ SolveMeta m vt
{-# INLINE solveMeta #-}

type MetaCtx = Bundle [MetaLookup, MetaState]

runMetaLookup ::
  Carrier m => (Meta -> Maybe Value) -> InterpreterFor MetaLookup m
runMetaLookup mlookup = interpret \case
  MetaLookup -> pure mlookup
{-# INLINE runMetaLookup #-}

type MetaLookupC s = InterpretC (ViaReifiedH s) MetaLookup
type MetaStateC s = InterpretC (ViaReifiedH s) MetaState
type MetaCtxToIOC m a =
  forall s1 s2.
  ( ReifiesHandler s1 MetaLookup (MetaStateC s2 m)
  , ReifiesHandler s2 MetaState m
  ) =>
  MetaLookupC s1 (MetaStateC s2 m) a

metaCtxToIO ::
  Eff (Embed IO) m => MetaCtxToIOC m a -> m (a, [(Meta, Maybe Value)])
metaCtxToIO m = do
  nextRef :: U.IOVector Int <- embed $ U.replicate 1 0
  solveds :: BasicHashTable Meta Value <- embed HT.new
  x <-
    interpret
      ( \case
          FreshMeta -> embed do
            next <- U.unsafeRead nextRef 0
            Meta next <$ U.unsafeWrite nextRef 0 (next + 1)
          SolveMeta meta t -> embed $ HT.insert solveds meta t
      )
      $ interpret
        \case
          MetaLookup -> pure $ unsafeDupablePerformIO . HT.lookup solveds
        m
  embed do
    next <- U.unsafeRead nextRef 0
    solvedList <- HT.toList solveds
    pure
      ( x
      , Map.toAscList $
          mconcat
            [ Just <$> Map.fromList solvedList
            , Map.fromDistinctAscList $ (,Nothing) <$> [0 .. Meta next - 1]
            ]
      )
{-# INLINE metaCtxToIO #-}
