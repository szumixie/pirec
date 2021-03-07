module Unnamed.Env (Env, empty, extend, index) where

import Relude hiding (empty)

import Data.Sequence qualified as Seq

import Unnamed.Var.Level (Level (..))

newtype Env a = Env (Seq a)
  deriving newtype (Show)

empty :: Env a
empty = Env Seq.empty

extend :: a -> Env a -> Env a
extend x (Env xs) = Env $ xs Seq.|> x

index :: Level -> Env a -> Maybe a
index (Level i) (Env xs) = Seq.lookup i xs
