module Unnamed.Data.Span (Span (..)) where

import Relude

import Optics

declareFieldLabels
  [d|
    data Span = Span
      { start :: {-# UNPACK #-} Int
      , end :: {-# UNPACK #-} Int
      }
      deriving stock (Show)
    |]
