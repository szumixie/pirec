module Pirec.Env (Env, empty, extend, index, level) where

import Relude hiding (empty)

import Data.Functor.Reverse (Reverse (..))

import Optics

import Pirec.Data.RList (RList)
import Pirec.Var.Level (Level (..))

newtype Env a = Env (RList a)
  deriving newtype (Show)
  deriving (Foldable) via Reverse RList

instance FoldableWithIndex Level Env where
  ifoldMap f (Env xs) =
    Reverse xs & ifoldMap \i -> f $ Level (length xs - i - 1)

empty :: Env a
empty = Env Empty

extend :: forall a. a -> Env a -> Env a
extend = coerce $ cons @(RList a)

index :: Level -> Env a -> Maybe a
index (Level i) (Env xs) = xs ^? ix (length xs - i - 1)

level :: forall a. Env a -> Level
level = coerce $ length @RList @a
