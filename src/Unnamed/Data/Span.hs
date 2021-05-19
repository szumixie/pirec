module Unnamed.Data.Span (Span (..), length) where

import Relude hiding (length)

import Optics

declareFieldLabels
  [d|
    data Span = Span
      { start :: Int
      , end :: Int
      }
      deriving stock (Show)
    |]

length :: Lens' Span Int
length = lensVL \f (Span start end) ->
  Span start . (+ start) <$> f (end - start)
{-# INLINE length #-}
