module Unnamed.Elab.Error (
  ElabError (..),
  ElabErrorType (..),
  prettyElabError,
) where

import Relude

import Control.Effect
import Data.Text.Prettyprint.Doc
import Text.Megaparsec (SourcePos, sourcePosPretty)

import Unnamed.Value (Value)
import Unnamed.Var.Name (Name)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Unify.Error (UnifyError, prettyUnifyError)
import Unnamed.Value.Pretty (prettyValue)

data ElabError
  = ElabError {-# UNPACK #-} SourcePos {-# UNPACK #-} Context ElabErrorType
  deriving stock (Show)

data ElabErrorType
  = UnifyError Value Value UnifyError
  | ScopeError {-# UNPACK #-} Name
  | DupField {-# UNPACK #-} Name
  | FieldMismatch (HashSet Name) (HashSet Name)
  | FieldExpected {-# UNPACK #-} Name Value
  deriving stock (Show)

prettyElabError :: Eff MetaLookup m => ElabError -> m (Doc ann)
prettyElabError (ElabError pos ctx err) = do
  perr <- case err of
    UnifyError va va' uerr -> do
      puerr <- prettyUnifyError ctx uerr
      pa <- prettyValue ctx va
      pa' <- prettyValue ctx va'
      pure $
        vsep
          [ puerr
          , "when unifying expected type:"
          , pa
          , "with inferred type:"
          , pa'
          ]
    ScopeError x -> pure $ "variable" <+> pretty x <+> "out of scope"
    DupField label -> pure $ "duplicate field" <+> pretty label <+> "in row"
    FieldMismatch tset aset ->
      pure $
        vsep
          [ "expected record with fields:"
          , pretty $ toList aset
          , "but got record with fields:"
          , pretty $ toList tset
          ]
    FieldExpected label va -> do
      pa <- prettyValue ctx va
      pure $ "expected field" <+> pretty label <+> "in type:" <> line <> pa
  pure $ pretty (sourcePosPretty pos) <> colon <> line <> perr
