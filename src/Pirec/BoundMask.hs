module Pirec.BoundMask (BoundMask, empty, extend, masked) where

import Relude hiding (empty)

import Optics

import Pirec.Env (Env)
import Pirec.Var.Level (Level (..))

newtype BoundMask = BoundMask IntSet
  deriving newtype (Show)

empty :: BoundMask
empty = BoundMask mempty

extend :: Level -> BoundMask -> BoundMask
extend (Level i) s = s & coercedTo @IntSet % contains i .~ True

masked :: BoundMask -> IxFold Level (Env a) a
masked (BoundMask s) = ifolded %& ifiltered \(Level i) _ -> s ^. contains i
{-# INLINE masked #-}
