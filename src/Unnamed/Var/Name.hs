module Unnamed.Var.Name (Name (..), name) where

import Data.Hashable (Hashable)
import Data.String (IsString)

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty, pretty)
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

newtype Name = Name ShortText
  deriving newtype (Show, IsString, Eq, Hashable)

instance Pretty Name where
  pretty (Name ts) = pretty $ TS.toText ts

name :: Text -> Name
name = Name . TS.fromText
