module Unnamed.Data.MultiMap (
  MultiMap (..),
  match,
  difference,
  superDifference,
  valid,
) where

import Relude

import Data.Align
import Data.HashMap.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.These
import Data.These.Optics
import GHC.Exts qualified

import Optics

newtype MultiMap k a = MultiMap (HashMap k (Seq a))
  deriving newtype (Show, Eq)
  deriving stock (Functor, Foldable, Traversable)
  deriving
    (FunctorWithIndex (k, Int), FoldableWithIndex (k, Int), Semialign, Align)
    via (Compose (HashMap k) Seq)

valid :: MultiMap k a -> Bool
valid (MultiMap xs) = not $ any null xs

instance TraversableWithIndex (k, Int) (MultiMap k) where
  itraverse f =
    fmap (coerce @(Compose (HashMap _) Seq _)) . itraverse f . coerce

instance (Eq k, Hashable k) => Semigroup (MultiMap k a) where
  MultiMap xs <> MultiMap ys = MultiMap $ M.unionWith (<>) xs ys

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

match ::
  (Eq k, Hashable k) =>
  (a -> b -> c) ->
  MultiMap k a ->
  MultiMap k b ->
  Maybe (MultiMap k c)
match f xs ys = traverse (previews _These (uncurry f)) (align xs ys)

elemDifference :: These (Seq a) (Seq b) -> Maybe (Maybe (Seq a))
elemDifference = \case
  This xs -> Just $ Just xs
  That _ -> Nothing
  These xs ys -> case length xs `compare` length ys of
    LT -> Nothing
    EQ -> Just Nothing
    GT -> Just . Just $ Seq.drop (length ys) xs

difference :: (Eq k, Hashable k) => MultiMap k a -> MultiMap k b -> MultiMap k a
difference (MultiMap xs) (MultiMap ys) =
  MultiMap $ M.mapMaybe (join . elemDifference) (align xs ys)

superDifference ::
  (Eq k, Hashable k) => MultiMap k a -> MultiMap k b -> Maybe (MultiMap k a)
superDifference (MultiMap xs) (MultiMap ys) =
  MultiMap . M.mapMaybe id <$> traverse elemDifference (align xs ys)
