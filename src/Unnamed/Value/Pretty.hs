module Unnamed.Value.Pretty (prettyValue) where

import GHC.Exts (sortWith)

import Data.Text.Prettyprint.Doc (Doc)
import Optics

import Unnamed.Elab.Context (Context (..))
import Unnamed.Eval (quote)
import Unnamed.Syntax.Core.Pretty (prettyTermWith)
import Unnamed.Syntax.Core.Pretty qualified as Pretty
import Unnamed.Value (Value)

prettyValue :: Context -> Value -> Doc ann
prettyValue (Context level _ names) = prettyTermWith pctx 0 . quote level
 where
  pctx =
    names
      & itoListOf (ifolded % _1)
      & sortWith snd
      & foldlOf' (folded % _1) (flip Pretty.extendCtx) Pretty.emptyCtx
