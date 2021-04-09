module Unnamed.Data.MultiMap (
  MultiMap (..),
  match,
  difference,
  superDifference,
) where

import Relude

import Data.Align
import Data.HashMap.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.These
import Data.These.Optics

import Optics

newtype MultiMap k a = MultiMap (HashMap k (Seq a))
  deriving newtype (Show, Monoid)
  deriving stock (Functor, Foldable, Traversable)
  deriving
    (FunctorWithIndex (k, Int), FoldableWithIndex (k, Int), Semialign, Align)
    via (Compose (HashMap k) Seq)

instance TraversableWithIndex (k, Int) (MultiMap k) where
  itraverse f =
    fmap (coerce @(Compose (HashMap _) Seq _)) . itraverse f . coerce

instance (Eq k, Hashable k) => Semigroup (MultiMap k a) where
  MultiMap xs <> MultiMap ys = MultiMap $ M.unionWith (<>) xs ys

instance Hashable k => One (MultiMap k a) where
  type OneItem (MultiMap k a) = (k, a)
  one (k, a) = MultiMap $ one (k, one a)

type instance Index (MultiMap k a) = (k, Int)
type instance IxValue (MultiMap k a) = a

instance (Eq k, Hashable k) => Ixed (MultiMap k a) where
  ix (k, i) = coercedTo @(HashMap k (Seq a)) % ix k % ix i

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
difference (MultiMap mx) (MultiMap my) =
  MultiMap $ M.mapMaybe (join . elemDifference) (align mx my)

superDifference ::
  (Eq k, Hashable k) => MultiMap k a -> MultiMap k b -> Maybe (MultiMap k a)
superDifference (MultiMap mx) (MultiMap my) =
  MultiMap . M.mapMaybe id <$> traverse elemDifference (align mx my)
