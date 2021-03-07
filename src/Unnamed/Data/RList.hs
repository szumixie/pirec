module Unnamed.Data.RList (RList, empty, cons, uncons) where

import Data.Foldable (toList)
import Optics hiding (cons, uncons)

data Tree a
  = Node ~a (Tree a) (Tree a)
  | Leaf ~a
  deriving stock (Functor, Foldable, Traversable)

data Trees a
  = TCons {-# UNPACK #-} Int (Tree a) (Trees a)
  | TNil
  deriving stock (Functor, Foldable, Traversable)

data RList a = RList {-# UNPACK #-} Int (Trees a)
  deriving stock (Functor, Traversable)

instance Foldable RList where
  foldMap f (RList _ ts) = foldMap f ts
  null (RList s _) = s == 0
  length (RList s _) = s

instance Show a => Show (RList a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

empty :: RList a
empty = RList 0 TNil

cons :: a -> RList a -> RList a
cons x (RList s ts0) = RList (s + 1) case ts0 of
  TCons w1 t1 (TCons w2 t2 ts)
    | w1 == w2 -> TCons (1 + w1 + w2) (Node x t1 t2) ts
  ts -> TCons 1 (Leaf x) ts

uncons :: RList a -> Maybe (a, RList a)
uncons (RList s ts0) = case ts0 of
  TNil -> Nothing
  TCons w t ts -> case t of
    Leaf x -> Just (x, RList (s - 1) ts)
    Node x t1 t2 ->
      let hw = w `div` 2
       in Just (x, RList (s - 1) . TCons hw t1 . TCons hw t2 $ ts)

type instance Index (RList a) = Int
type instance IxValue (RList a) = a

ixTree :: Int -> Int -> AffineTraversal' (Tree a) a
ixTree i0 w0 = atraversalVL \pure' f ->
  let go i w = \case
        Leaf x
          | i == 0 -> Leaf <$> f x
          | otherwise -> pure' $ Leaf x
        Node x t1 t2
          | i == 0 -> f x <&> \x' -> Node x' t1 t2
          | i <= hw -> go (i - 1) hw t1 <&> \t1' -> Node x t1' t2
          | otherwise -> go (i - 1 - hw) hw t2 <&> \t2' -> Node x t1 t2'
       where
        hw = w `div` 2
   in go i0 w0

instance Ixed (RList a) where
  ix i0 = atraversalVL \pure' f (RList s ts0) ->
    let go i = \case
          TNil -> pure' TNil
          TCons w t ts
            | i < w ->
              t & atraverseOf (ixTree i w) pure' f <&> \t' -> TCons w t' ts
            | otherwise -> TCons w t <$> go (i - w) ts
     in RList s <$> go i0 ts0
