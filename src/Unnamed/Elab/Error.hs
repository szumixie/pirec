module Unnamed.Elab.Error (
  ElabError (..),
  ElabErrorType (..),
  prettyElabError,
) where

import Relude

import Data.Text.Prettyprint.Doc
import Optics (declareFieldLabels)
import Text.Megaparsec (SourcePos, sourcePosPretty)

import Unnamed.Elab.Context (Context)
import Unnamed.Value (Value)
import Unnamed.Value.Pretty (prettyValue)
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data ElabError = ElabError
      { pos :: {-# UNPACK #-} SourcePos
      , ctx :: {-# UNPACK #-} Context
      , err :: ElabErrorType
      }
      deriving stock (Show)

    data ElabErrorType
      = ConvError {expected :: Value, inferred :: Value}
      | ScopeError {name :: {-# UNPACK #-} Name}
      | LamInference
      | PiExpected {typ :: Value}
      deriving stock (Show)
    |]

prettyElabError :: ElabError -> Doc ann
prettyElabError (ElabError pos ctx err) =
  pretty (sourcePosPretty pos) <> colon <> line <> case err of
    ConvError a a' ->
      vsep
        [ "expected type:"
        , prettyValue ctx a
        , "but got inferred type:"
        , prettyValue ctx a'
        ]
    ScopeError x -> "variable" <+> pretty x <+> "out of scope"
    LamInference -> "cannot infer type of lambda"
    PiExpected a ->
      "expected function type but got inferred type:" <+> prettyValue ctx a
