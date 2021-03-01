module Unnamed.Var.Name (Name (..), name) where

import Data.Hashable (Hashable)
import Data.String (IsString)

import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

import Data.Text.Prettyprint.Doc (Pretty, pretty)
import Optics

declareFieldLabels
  [d|
    newtype Name = Name {shortText :: ShortText}
      deriving newtype (Show, IsString, Eq, Hashable)
    |]

instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "text" k Name Name a b
  where
  labelOptic = #shortText % iso TS.toText TS.fromText

instance Pretty Name where
  pretty = pretty . view #text

name :: Text -> Name
name = review #text
