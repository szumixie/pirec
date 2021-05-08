module Unnamed.Unify.Error (UnifyError (..), prettyUnifyError) where

import Relude

import Control.Effect
import Data.Text.Prettyprint.Doc

import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Meta (Meta)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Pretty (prettyValue)

data UnifyError
  = Mismatch Value Value
  | ScopeError Meta Level
  | OccursError Meta
  | Nonlinear Level
  | Nonvariable Value
  | NonInvertable
  deriving stock (Show)

prettyUnifyError :: Eff MetaLookup m => Context -> UnifyError -> m (Doc ann)
prettyUnifyError ctx = \case
  Mismatch a a' -> do
    a <- prettyValue ctx a
    a' <- prettyValue ctx a'
    pure $
      vsep
        [ "expected type:"
        , indent 2 a
        , "but got inferred type:"
        , indent 2 a'
        ]
  ScopeError meta lvl -> do
    x <- prettyValue ctx $ V.var lvl
    pure $ "variable" <+> x <+> "not in scope when solving" <+> pretty meta
  OccursError meta -> pure $ "occurs check failed when solving" <+> pretty meta
  Nonlinear lvl -> do
    x <- prettyValue ctx $ V.var lvl
    pure $
      "ambiguous hole due to multiple instances of variable" <+> x
        <+> "in the context"
  Nonvariable t -> do
    t <- prettyValue ctx t
    pure $ "got nonvariable in the context:" <> line <> indent 2 t
  NonInvertable -> pure "got non-invertable spine"
