module Pirec.Var.Name (Name (.., Wildcard), name) where

import Relude

import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

import Data.Text.Prettyprint.Doc (Pretty, pretty)

newtype Name = Name ShortText
  deriving newtype (Show, IsString, Eq, Hashable)

pattern Wildcard :: Name
pattern Wildcard = "_"

instance Pretty Name where
  pretty = pretty . TS.toText . coerce

name :: Text -> Name
name = coerce TS.fromText
