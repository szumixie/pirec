module Unnamed.Value.Pretty (prettyValue) where

import Data.Text.Prettyprint.Doc
import Unnamed.Elab.Context (Context)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Eval (quote)
import Unnamed.Syntax.Core.Pretty (prettyTermWith)
import Unnamed.Value (Value)

prettyValue :: Context -> Value -> Doc ann
prettyValue ctx =
  prettyTermWith (Ctx.getNameEnv ctx) (Ctx.getNameSet ctx) 0
    . quote (Ctx.level ctx)
