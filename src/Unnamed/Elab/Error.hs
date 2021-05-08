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

import Unnamed.Data.Span qualified as Span
import Unnamed.Elab.Context (Context)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Plicity (Plicity)
import Unnamed.Value (Value)
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

import Unnamed.Effect.Meta
import Unnamed.Pretty (prettyValue)
import Unnamed.Unify.Error (UnifyError, prettyUnifyError)

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
        UnifyError a a' uerr -> do
          puerr <- prettyUnifyError ctx uerr
          a <- prettyValue ctx a
          a' <- prettyValue ctx a'
          pure $
            vsep
              [ puerr
              , "when unifying expected type:"
              , indent 2 a
              , "with inferred type:"
              , indent 2 a'
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
