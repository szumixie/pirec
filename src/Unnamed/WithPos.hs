module Unnamed.WithPos (WithPos (..), withPos) where

import Relude

import Optics
import Text.Megaparsec (SourcePos)

declareFieldLabels
  [d|
    data WithPos a = WithPos {pos :: SourcePos, value :: a}
      deriving stock (Show)
    |]

withPos :: (SourcePos -> a -> r) -> WithPos a -> r
withPos f (WithPos pos x) = f pos x
