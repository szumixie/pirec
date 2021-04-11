module Unnamed.Data.MultiMapAlter (
  MultiMapAlter,
  singleInsert,
  singleDelete,
  apply,
  lookup,
  match,
  ifoldedAlter,
) where

import Relude

import Data.Align
import Data.HashMap.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.These
import Data.Traversable (for)

import Optics

import Unnamed.Data.MultiMap (MultiMap (..))

data ElemAlter a = ElemAlter {-# UNPACK #-} Int (Seq a)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Semigroup (ElemAlter a) where
  ElemAlter i xs <> ElemAlter j ys = ElemAlter (i' + j) (xs <> ys')
   where
    ElemAlter i' ys' = elemDrop i ys

instance Monoid (ElemAlter a) where
  mempty = ElemAlter 0 mempty

elemIsId :: ElemAlter a -> Bool
elemIsId (ElemAlter i xs) = i <= 0 && null xs

elemInsert :: Seq a -> ElemAlter a
elemInsert = ElemAlter 0

elemDelete :: Int -> ElemAlter a
elemDelete i = ElemAlter i mempty

elemDrop :: Int -> Seq a -> ElemAlter a
elemDrop i xs = case i `compare` length xs of
  LT -> elemInsert (Seq.drop i xs)
  EQ -> mempty
  GT -> elemDelete (i - length xs)

newtype MultiMapAlter k a = MultiMapAlter (HashMap k (ElemAlter a))
  deriving newtype (Show)
  deriving stock (Functor, Foldable, Traversable)

instance (Eq k, Hashable k) => Semigroup (MultiMapAlter k a) where
  MultiMapAlter mx <> MultiMapAlter my =
    MultiMapAlter $
      align mx my & M.mapMaybe \case
        This xs -> Just xs
        That ys -> Just ys
        These xs ys -> guarded (not . elemIsId) (xs <> ys)

instance (Eq k, Hashable k) => Monoid (MultiMapAlter k a) where
  mempty = MultiMapAlter mempty

singleInsert :: Hashable k => k -> a -> MultiMapAlter k a
singleInsert k x = MultiMapAlter $ one (k, elemInsert (one x))

singleDelete :: Hashable k => k -> MultiMapAlter k a
singleDelete k = MultiMapAlter $ one (k, elemDelete 1)

apply :: (Eq k, Hashable k) => MultiMapAlter k a -> MultiMap k a -> MultiMap k a
apply (MultiMapAlter mx) (MultiMap my) =
  MultiMap $
    align mx my & M.mapMaybe \case
      This (ElemAlter _ xs) -> guarded (not . null) xs
      That ys -> Just ys
      These (ElemAlter i xs) ys -> guarded (not . null) (xs <> Seq.drop i ys)

lookup :: (Eq k, Hashable k) => k -> Int -> MultiMapAlter k a -> Either Int a
lookup k i (MultiMapAlter m) =
  m & views (at k) \case
    Nothing -> Left i
    Just (ElemAlter j xs) -> xs ^? ix i & maybeToRight (i - length xs + j)

match ::
  (Eq k, Hashable k) =>
  (a -> b -> c) ->
  MultiMapAlter k a ->
  MultiMapAlter k b ->
  Maybe (MultiMapAlter k c)
match f (MultiMapAlter mx) (MultiMapAlter my) =
  coerce $ for (align mx my) \case
    These (ElemAlter i xs) (ElemAlter j ys)
      | i == j && length xs == length ys ->
        Just $ ElemAlter i (Seq.zipWith f xs ys)
    _ -> Nothing

ifoldedAlter :: IxFold k (MultiMapAlter k a) (Maybe a)
ifoldedAlter =
  coercedTo @(HashMap _ _) % ifolded % foldVL \f (ElemAlter i xs) ->
    traverse_ (f . Just) xs *> replicateM_ i (f Nothing)
