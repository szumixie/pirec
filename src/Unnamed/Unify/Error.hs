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
import Unnamed.Value.Pretty (prettyValue)

data UnifyError
  = Mismatch Value Value
  | ScopeError Level
  | OccursError Meta
  | Nonlinear Level
  | Nonvariable Value
  | NonInvertable
  deriving stock (Show)

prettyUnifyError :: Eff MetaLookup m => Context -> UnifyError -> m (Doc ann)
prettyUnifyError ctx = \case
  Mismatch va va' -> do
    pa <- prettyValue ctx va
    pa' <- prettyValue ctx va'
    pure $ vsep ["expected type:", pa, "but got inferred type:", pa']
  ScopeError lvl -> do
    px <- prettyValue ctx $ V.var lvl
    pure $ "variable" <+> px <+> "not in scope"
  OccursError meta -> pure $ "occurs check failed when solving" <+> pretty meta
  Nonlinear lvl -> do
    px <- prettyValue ctx $ V.var lvl
    pure $
      "ambiguous hole due to multiple instances of variable" <+> px
        <+> "in the context"
  Nonvariable vt -> do
    pt <- prettyValue ctx vt
    pure $ "got nonvariable in the context:" <> line <> pt
  NonInvertable -> pure "got non-invertable spine"
