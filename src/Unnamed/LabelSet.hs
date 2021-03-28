module Unnamed.LabelSet (
  LabelSet (..),
  isSubsetOf,
  disjoint,
  full,
  intersection,
) where

import Relude

import Data.HashSet qualified as Set

import Optics

import Unnamed.Var.Name

data LabelSet
  = Has (HashSet Name)
  | Lacks (HashSet Name)
  deriving stock (Show)

instance Semigroup LabelSet where
  Has xs <> Has ys = Has $ xs <> ys
  Has xs <> Lacks ys = Lacks $ Set.difference ys xs
  Lacks xs <> Has ys = Lacks $ Set.difference xs ys
  Lacks xs <> Lacks ys = Lacks $ Set.intersection xs ys

instance Monoid LabelSet where
  mempty = Has mempty

disjointHashSet :: HashSet Name -> HashSet Name -> Bool
disjointHashSet xs ys = xs & all \x -> not $ ys ^. contains x

isSubsetOf :: LabelSet -> LabelSet -> Bool
isSubsetOf = curry \case
  (Has xs, Has ys) -> xs `Set.isSubsetOf` ys
  (Has xs, Lacks ys) -> disjointHashSet xs ys
  (Lacks xs, Has ys) -> disjointHashSet xs ys
  (Lacks xs, Lacks ys) -> ys `Set.isSubsetOf` xs

disjoint :: LabelSet -> LabelSet -> Bool
disjoint = curry \case
  (Has xs, Has ys) -> disjointHashSet xs ys
  (Has xs, Lacks ys) -> ys `Set.isSubsetOf` xs
  (Lacks xs, Has ys) -> xs `Set.isSubsetOf` ys
  (Lacks _, Lacks _) -> False

full :: LabelSet
full = Lacks mempty

intersection :: LabelSet -> LabelSet -> LabelSet
intersection = curry \case
  (Has xs, Has ys) -> Has $ Set.intersection xs ys
  (Has xs, Lacks ys) -> Has $ Set.difference xs ys
  (Lacks xs, Has ys) -> Has $ Set.difference ys xs
  (Lacks xs, Lacks ys) -> Lacks $ xs <> ys
