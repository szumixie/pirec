module Pirec.Unify.Error (UnifyError (..), prettyUnifyError) where

import Relude

import Control.Effect
import Prettyprinter

import Pirec.Value (Value)
import Pirec.Value qualified as V
import Pirec.Var.Level (Level)
import Pirec.Var.Meta (Meta)

import Pirec.Effect.Meta
import Pirec.Elab.Context (Context)
import Pirec.Pretty (prettyValue)

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
  Mismatch inferred expected -> do
    inferred <- prettyValue ctx inferred
    expected <- prettyValue ctx expected
    pure $
      vsep
        [ "expected type:"
        , indent 2 expected
        , "but got inferred type:"
        , indent 2 inferred
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
    pure $
      vsep
        [ "got nonvariable in the context:"
        , indent 2 t
        ]
  NonInvertable -> pure "got non-invertable spine"
