module Pirec.Elab.Error (
  ElabError (..),
  ElabErrorType (..),
  prettyElabError,
) where

import Relude

import Control.Effect
import Optics
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec

import Pirec.Data.Span qualified as Span
import Pirec.Elab.Context (Context)
import Pirec.Elab.Context qualified as Ctx
import Pirec.Plicity (Plicity)
import Pirec.Value (Value)
import Pirec.Var.Meta (Meta)
import Pirec.Var.Name (Name)

import Pirec.Effect.Meta
import Pirec.Pretty (prettyValue)
import Pirec.Unify.Error (UnifyError, prettyUnifyError)

data ElabErrorType
  = UnifyError Value Value UnifyError
  | ScopeError Name
  | PlicityMismatch Plicity Plicity
  deriving stock (Show)

declareFieldLabels
  [d|
    data ElabError = ElabError
      { context :: Context
      , errorType :: ElabErrorType
      }
      deriving stock (Show)
    |]

declareFieldLabels
  [d|
    data CompElabError = CompElabError
      { metaLookup :: ~(Meta -> Maybe Value)
      , error :: ElabError
      }
    |]

instance Eq CompElabError where
  _ == _ = True

instance Ord CompElabError where
  compare _ _ = EQ

instance ShowErrorComponent CompElabError where
  showErrorComponent (CompElabError mlookup (ElabError ctx err)) =
    renderString . layoutPretty defaultLayoutOptions . run $ runMetaLookup
      mlookup
      case err of
        UnifyError inferred expected uerr -> do
          puerr <- prettyUnifyError ctx uerr
          inferred <- prettyValue ctx inferred
          expected <- prettyValue ctx expected
          pure $
            vsep
              [ puerr
              , "when unifying expected type:"
              , indent 2 expected
              , "with inferred type:"
              , indent 2 inferred
              ]
        ScopeError x -> pure $ "variable" <+> pretty x <+> "out of scope"
        PlicityMismatch pl pl' ->
          pure $
            hsep
              [ "got"
              , pretty pl
              , "application when"
              , pretty pl'
              , "application was expected"
              ]
  errorComponentLen = view $ #error % #context % Ctx.span % Span.length

prettyElabError :: Eff MetaLookup m => ElabError -> FilePath -> Text -> m String
prettyElabError err fp input = do
  mlookup <- metaLookup
  pure . errorBundlePretty $
    ParseErrorBundle
      ( one $
          FancyError
            (err ^. #context % Ctx.span % #start)
            (one $ ErrorCustom (CompElabError mlookup err))
      )
      (PosState input 0 (initialPos fp) defaultTabWidth "")
