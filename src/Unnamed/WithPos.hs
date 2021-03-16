module Unnamed.WithPos (WithPos (..)) where

import Relude

import Text.Megaparsec (SourcePos)

data WithPos a = WithPos SourcePos a
  deriving stock (Show)
