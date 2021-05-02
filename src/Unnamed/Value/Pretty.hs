module Unnamed.Value.Pretty (prettyValue) where

import Relude

import Control.Effect
import Data.Text.Prettyprint.Doc (Doc)
import Optics

import Unnamed.Syntax.Core.Pretty (prettyTermWith)
import Unnamed.Syntax.Core.Pretty.Context qualified as Pretty
import Unnamed.Value (Value)

import Unnamed.Effect.Meta
import Unnamed.Elab.Context (Context)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Eval (quote)

prettyValue :: Eff MetaLookup m => Context -> Value -> m (Doc ann)
prettyValue ctx = fmap (prettyTermWith pctx 0) . quote (Ctx.level ctx)
 where
  pctx =
    ctx ^. #names
      & itoListOf (ifolded % _1)
      & sortWith snd
      & foldlOf' (folded % _1) (flip Pretty.extend) Pretty.empty
