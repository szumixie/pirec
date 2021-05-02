module Unnamed.Syntax.Core.Pretty.Context (Context, empty, extend) where

import Relude hiding (empty)

import Optics

import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Var.Name (Name (..))

declareFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  [d|
    data Context = Context
      { env :: Env Name
      , names :: HashSet Name
      }
      deriving stock (Show)
    |]

empty :: Context
empty = Context Env.empty mempty

extend :: Name -> Context -> Context
extend name (Context env names) =
  Context (env & Env.extend name) (names & contains name .~ True)
