module Unnamed.Value.Pretty (prettyValue) where

import Relude

import Control.Effect
import Data.Text.Prettyprint.Doc (Doc)

import Unnamed.Syntax.Core.Pretty (prettyTermWith)
import Unnamed.Value (Value)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Eval (quote)

prettyValue :: Eff MetaLookup m => Context -> Value -> m (Doc ann)
prettyValue ctx =
  fmap (prettyTermWith (Ctx.prettyCtx ctx) 0) . quote (Ctx.level ctx)
