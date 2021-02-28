module Unnamed.WithPos (WithPos (..), withPos) where

import Text.Megaparsec (SourcePos)

data WithPos a = WithPos SourcePos a
  deriving stock (Show)

withPos :: (SourcePos -> a -> r) -> WithPos a -> r
withPos f (WithPos pos x) = f pos x
