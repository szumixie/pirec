module Unnamed.Env (Env, empty, extend, index, level) where

import Relude hiding (empty)

import Optics

import Unnamed.Var.Level (Level (..))

newtype Env a = Env (Seq a)
  deriving newtype (Show, Foldable)

instance FoldableWithIndex Level Env where
  ifoldMap f (Env xs) = ifoldMap (f . coerce) xs

empty :: Env a
empty = Env mempty

extend :: a -> Env a -> Env a
extend x (Env xs) = Env $ xs |> x

index :: Level -> Env a -> Maybe a
index (Level i) (Env xs) = xs ^? ix i

level :: Env a -> Level
level (Env xs) = Level $ length xs
