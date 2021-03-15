module Unnamed.Elab.Error (
  ElabError (..),
  ElabErrorType (..),
  prettyElabError,
) where

import Relude

import Control.Effect
import Data.Text.Prettyprint.Doc
import Optics (declareFieldLabels)
import Text.Megaparsec (SourcePos, sourcePosPretty)

import Unnamed.Value (Value)
import Unnamed.Var.Name (Name)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Unify.Error (UnifyError, prettyUnifyError)
import Unnamed.Value.Pretty (prettyValue)

declareFieldLabels
  [d|
    data ElabError = ElabError
      { pos :: {-# UNPACK #-} SourcePos
      , context :: {-# UNPACK #-} Context
      , error :: ElabErrorType
      }
      deriving stock (Show)

    data ElabErrorType
      = UnifyError {expected :: Value, inferred :: Value, error :: UnifyError}
      | ScopeError {name :: {-# UNPACK #-} Name}
      deriving stock (Show)
    |]

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
  pure $ pretty (sourcePosPretty pos) <> colon <> line <> perr
