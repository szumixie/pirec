module Pirec.Data.MultiMap (
  MultiMap (..),
  intersectionWith,
  matchWith,
  difference,
  isSubsetOf,
  valid,
) where

import Relude hiding (Alt, catMaybes, mapMaybe)

import Data.Align
import Data.Functor.Plus
import Data.HashMap.Strict qualified as M
import Data.These.Optics
import GHC.Exts qualified
import Witherable

import Optics

newtype MultiMap k a = MultiMap (HashMap k (Seq a))
  deriving newtype (Show, Eq)
  deriving stock (Functor, Foldable, Traversable)
  deriving
    (FunctorWithIndex (k, Int), FoldableWithIndex (k, Int), Semialign, Align)
    via (Compose (HashMap k) Seq)
  deriving anyclass
    (Witherable, FilterableWithIndex (k, Int), WitherableWithIndex (k, Int))

valid :: MultiMap k a -> Bool
valid (MultiMap m) = not $ any null m

instance TraversableWithIndex (k, Int) (MultiMap k) where
  itraverse f =
    fmap (coerce @(Compose (HashMap _) Seq _)) . itraverse f . coerce

instance (Eq k, Hashable k) => Filterable (MultiMap k) where
  mapMaybe f (MultiMap m) =
    MultiMap $ mapMaybe (guarded (not . null) . mapMaybe f) m

instance (Eq k, Hashable k) => Apply (MultiMap k) where
  liftF2 = intersectionWith

instance (Eq k, Hashable k) => Alt (MultiMap k) where
  (<!>) = (<>)

instance (Eq k, Hashable k) => Plus (MultiMap k) where
  zero = mempty

instance (Eq k, Hashable k) => Semigroup (MultiMap k a) where
  (<>) = coerce $ M.unionWith @k @(Seq a) (<>)

instance (Eq k, Hashable k) => Monoid (MultiMap k a) where
  mempty = MultiMap mempty

instance Hashable k => One (MultiMap k a) where
  type OneItem (MultiMap k a) = (k, a)
  one (k, a) = MultiMap $ one (k, one a)

instance (Eq k, Hashable k) => IsList (MultiMap k a) where
  type Item (MultiMap k a) = (k, a)
  fromList = foldl' (\xs x -> xs <> one x) mempty
  toList = fmap (_1 %~ fst) . itoList

type instance Index (MultiMap k a) = (k, Int)
type instance IxValue (MultiMap k a) = a

instance (Eq k, Hashable k) => Ixed (MultiMap k a) where
  ix (k, i) = coercedTo @(HashMap k (Seq a)) % ix k % ix i
  {-# INLINE ix #-}

intersectionWith ::
  (Eq k, Hashable k) =>
  (a -> b -> c) ->
  MultiMap k a ->
  MultiMap k b ->
  MultiMap k c
intersectionWith f mx my = mapMaybe (previews _These $ uncurry f) $ align mx my

matchWith ::
  (Eq k, Hashable k) =>
  (a -> b -> c) ->
  MultiMap k a ->
  MultiMap k b ->
  Maybe (MultiMap k c)
matchWith f mx my = traverse (previews _These $ uncurry f) $ align mx my

difference :: (Eq k, Hashable k) => MultiMap k a -> MultiMap k b -> MultiMap k a
difference mx my = mapMaybe (preview _This) $ align mx my

isSubsetOf :: (Eq k, Hashable k) => MultiMap k a -> MultiMap k b -> Bool
isSubsetOf mx my = all (has there) $ align mx my
