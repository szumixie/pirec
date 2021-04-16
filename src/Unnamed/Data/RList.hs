module Unnamed.Data.RList (RList, empty, cons, uncons) where

import Relude hiding (empty, uncons)

import Data.Traversable (fmapDefault, foldMapDefault)
import GHC.Exts qualified
import Text.Show

import Optics hiding (cons, uncons)

data Tree a
  = Node ~a (Tree a) (Tree a)
  | Leaf ~a

data Trees a
  = TCons {-# UNPACK #-} Int (Tree a) (Trees a)
  | TNil

data RList a = RList {-# UNPACK #-} Int (Trees a)
  deriving stock (Functor, Traversable)

instance Show a => Show (RList a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f = go
   where
    go = \case
      Leaf x -> Leaf <$> f x
      Node x l r -> Node <$> f x <*> go l <*> go r
  {-# INLINEABLE traverse #-}

instance Functor Trees where
  fmap = fmapDefault

instance Foldable Trees where
  foldMap = foldMapDefault

instance Traversable Trees where
  traverse f = go
   where
    go = \case
      TNil -> pure TNil
      TCons w t ts -> TCons w <$> traverse f t <*> go ts
  {-# INLINEABLE traverse #-}

instance Foldable RList where
  foldMap f (RList _ ts) = foldMap f ts
  {-# INLINE foldMap #-}
  null (RList s _) = s == 0
  {-# INLINE null #-}
  length (RList s _) = s
  {-# INLINE length #-}

itraverseTree ::
  Applicative f => (Int -> a -> f b) -> Int -> Int -> Tree a -> f (Tree b)
itraverseTree f = go
 where
  go !i !w = \case
    Leaf x -> Leaf <$> f i x
    Node x t1 t2 ->
      Node <$> f i x <*> go (i + 1) hw t1 <*> go (i + 1 + hw) hw t2
     where
      hw = w `div` 2
{-# INLINEABLE itraverseTree #-}
{-# SPECIALIZE itraverseTree ::
  (Int -> a -> Identity b) -> Int -> Int -> Tree a -> Identity (Tree b)
  #-}
{-# SPECIALIZE itraverseTree ::
  Monoid m =>
  (Int -> a -> Const m b) ->
  Int ->
  Int ->
  Tree a ->
  Const m (Tree b)
  #-}

instance FunctorWithIndex Int RList where
  imap :: forall a b. (Int -> a -> b) -> RList a -> RList b
  imap = coerce $ itraverse @_ @RList @Identity @a @b
  {-# INLINE imap #-}

instance FoldableWithIndex Int RList where
  ifoldMap :: forall m a. Monoid m => (Int -> a -> m) -> RList a -> m
  ifoldMap = coerce $ itraverse @_ @RList @(Const m) @a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Int RList where
  itraverse f (RList s ts) = RList s <$> go 0 ts
   where
    go !i = \case
      TNil -> pure TNil
      TCons w t ts -> TCons w <$> itraverseTree f i w t <*> go (i + w) ts
  {-# INLINEABLE itraverse #-}
  {-# SPECIALIZE itraverse ::
    (Int -> a -> Identity b) -> RList a -> Identity (RList b)
    #-}
  {-# SPECIALIZE itraverse ::
    Monoid m => (Int -> a -> Const m b) -> RList a -> Const m (RList b)
    #-}

empty :: RList a
empty = RList 0 TNil

cons :: a -> RList a -> RList a
cons x (RList s ts) = RList (s + 1) case ts of
  TCons w1 t1 (TCons w2 t2 ts)
    | w1 == w2 -> TCons (1 + w1 + w2) (Node x t1 t2) ts
  _ -> TCons 1 (Leaf x) ts

uncons :: RList a -> Maybe (a, RList a)
uncons (RList s ts) = case ts of
  TNil -> Nothing
  TCons w t ts -> case t of
    Leaf x -> Just (x, RList (s - 1) ts)
    Node x t1 t2 -> Just (x, RList (s - 1) . TCons hw t1 . TCons hw t2 $ ts)
     where
      hw = w `div` 2

instance AsEmpty (RList a) where
  _Empty = prism' (const empty) (null >>> bool Nothing (Just ()))

instance Cons (RList a) (RList b) a b where
  _Cons = prism (uncurry cons) (uncons >>> maybeToRight empty)

instance IsList (RList a) where
  type Item (RList a) = a
  fromList = foldr cons empty
  toList = toList

type instance Index (RList a) = Int
type instance IxValue (RList a) = a

ixTreeVL ::
  Functor f =>
  (forall r. r -> f r) ->
  (a -> f a) ->
  Int ->
  Int ->
  Tree a ->
  f (Tree a)
ixTreeVL pure f = go
 where
  go !i !w = \case
    Leaf x
      | i == 0 -> Leaf <$> f x
      | otherwise -> pure $ Leaf x
    Node x t1 t2
      | i == 0 -> f x <&> \x -> Node x t1 t2
      | i <= hw -> go (i - 1) hw t1 <&> \t1 -> Node x t1 t2
      | otherwise -> go (i - 1 - hw) hw t2 <&> \t2 -> Node x t1 t2
     where
      hw = w `div` 2
{-# INLINEABLE ixTreeVL #-}
{-# SPECIALIZE ixTreeVL ::
  (forall r. r -> Identity r) ->
  (a -> Identity a) ->
  Int ->
  Int ->
  Tree a ->
  Identity (Tree a)
  #-}
{-# SPECIALIZE ixTreeVL ::
  (forall r. r -> Const m r) ->
  (a -> Const m a) ->
  Int ->
  Int ->
  Tree a ->
  Const m (Tree a)
  #-}

ixVL ::
  Functor f =>
  Int ->
  (forall r. r -> f r) ->
  (a -> f a) ->
  RList a ->
  f (RList a)
ixVL i pure f (RList s ts) = RList s <$> go i ts
 where
  go !i = \case
    TNil -> pure TNil
    TCons w t ts
      | i < w -> ixTreeVL pure f i w t <&> \t -> TCons w t ts
      | otherwise -> TCons w t <$> go (i - w) ts
{-# INLINEABLE ixVL #-}
{-# SPECIALIZE ixVL ::
  Int ->
  (forall r. r -> Identity r) ->
  (a -> Identity a) ->
  RList a ->
  Identity (RList a)
  #-}
{-# SPECIALIZE ixVL ::
  Int ->
  (forall r. r -> Const m r) ->
  (a -> Const m a) ->
  RList a ->
  Const m (RList a)
  #-}

instance Ixed (RList a) where
  ix i = atraversalVL (ixVL i)
  {-# INLINE ix #-}
