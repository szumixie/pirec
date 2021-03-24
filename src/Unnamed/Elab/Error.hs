module Unnamed.Elab.Error (
  ElabError (..),
  ElabErrorType (..),
  prettyElabError,
) where

import Relude

import Control.Effect
import Data.Text.Prettyprint.Doc
import Optics
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec hiding (label)

import Unnamed.Data.Span (Span (Span))
import Unnamed.Value (Value)
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Unify.Error (UnifyError, prettyUnifyError)
import Unnamed.Value.Pretty (prettyValue)

data ElabErrorType
  = UnifyError Value Value UnifyError
  | ScopeError {-# UNPACK #-} Name
  | DupField {-# UNPACK #-} Name
  | FieldMismatch (HashSet Name) (HashSet Name)
  | FieldExpected {-# UNPACK #-} Name Value
  deriving stock (Show)

declareFieldLabels
  [d|
    data ElabError = ElabError
      { context :: {-# UNPACK #-} Context
      , errorType :: ElabErrorType
      }
      deriving stock (Show)
    |]

declareFieldLabels
  [d|
    data CompElabError = CompElabError
      { metaLookup :: ~(Meta -> Maybe Value)
      , error :: {-# UNPACK #-} ElabError
      }
    |]

instance Eq CompElabError where
  _ == _ = True

instance Ord CompElabError where
  compare _ _ = EQ

instance ShowErrorComponent CompElabError where
  showErrorComponent (CompElabError mget (ElabError ctx err)) =
    renderString . layoutPretty defaultLayoutOptions . run $ runMetaLookup
      mget
      case err of
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
  errorComponentLen err = end - start
   where
    Span start end = err ^. #error % #context % #span

prettyElabError :: Eff MetaLookup m => ElabError -> FilePath -> Text -> m String
prettyElabError err fp input = do
  mget <- metaLookup
  pure . errorBundlePretty $
    ParseErrorBundle
      ( one $
          FancyError
            (err ^. #context % #span % #start)
            (one $ ErrorCustom (CompElabError mget err))
      )
      (PosState input 0 (initialPos fp) defaultTabWidth "")
