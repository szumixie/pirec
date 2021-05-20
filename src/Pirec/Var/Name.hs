module Pirec.Var.Name (Name (..), _Name, name) where

import Relude

import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

import Data.Text.Prettyprint.Doc (Pretty, pretty)
import Optics

newtype Name = Name ShortText
  deriving newtype (Show, IsString, Eq, Hashable)

_Name :: Iso' Name Text
_Name = coerced % iso TS.toText TS.fromText
{-# INLINE _Name #-}

instance Pretty Name where
  pretty = pretty . view _Name

name :: Text -> Name
name = review _Name
