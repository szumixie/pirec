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
import Data.Mutable
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
  nextRef :: IOPRef Int <- embed $ newRef 1
  solveds :: BasicHashTable Meta Value <- embed HT.new
  interpret
    ( \case
        FreshMeta -> embed do
          next <- readRef nextRef
          Meta next <$ writeRef nextRef (next + 1)
        SolveMeta meta vt -> embed $ HT.insert solveds meta vt
    )
    $ interpret
      \case
        MetaLookup -> pure $ unsafeDupablePerformIO . HT.lookup solveds
      m
