module Unnamed.Effect.Meta (
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
import Data.Vector.Unboxed.Mutable qualified as U
import System.IO.Unsafe (unsafeDupablePerformIO)

import Control.Effect

import Unnamed.Value (Value)
import Unnamed.Var.Meta (Meta (..))

data MetaLookup :: Effect where
  MetaLookup :: MetaLookup m (Meta -> Maybe Value)

metaLookup :: Eff MetaLookup m => m (Meta -> Maybe Value)
metaLookup = send MetaLookup

data MetaState :: Effect where
  FreshMeta :: MetaState m Meta
  SolveMeta :: Meta -> Value -> MetaState m ()

freshMeta :: Eff MetaState m => m Meta
freshMeta = send FreshMeta

solveMeta :: Eff MetaState m => Meta -> Value -> m ()
solveMeta m vt = send $ SolveMeta m vt

type MetaCtx = Bundle [MetaLookup, MetaState]

runMetaLookup ::
  Carrier m => (Meta -> Maybe Value) -> InterpreterFor MetaLookup m
runMetaLookup mlookup = interpret \case
  MetaLookup -> pure mlookup

type MetaLookupC s = InterpretC (ViaReifiedH s) MetaLookup
type MetaStateC s = InterpretC (ViaReifiedH s) MetaState
type MetaCtxToIOC m a =
  forall s1 s2.
  ( ReifiesHandler s1 MetaLookup (MetaStateC s2 m)
  , ReifiesHandler s2 MetaState m
  ) =>
  MetaLookupC s1 (MetaStateC s2 m) a

metaCtxToIO :: Eff (Embed IO) m => MetaCtxToIOC m a -> m a
metaCtxToIO m = do
  nextRef :: U.IOVector Int <- embed $ U.replicate 1 1
  solveds :: BasicHashTable Meta Value <- embed HT.new
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
