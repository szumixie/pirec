module Pirec.Label (Label (..), label) where

import Relude

import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

import Data.Text.Prettyprint.Doc (Pretty, pretty)

newtype Label = Label ShortText
  deriving newtype (Show, IsString, Eq, Hashable)

instance Pretty Label where
  pretty = pretty . TS.toText . coerce

label :: Text -> Label
label = coerce TS.fromText
