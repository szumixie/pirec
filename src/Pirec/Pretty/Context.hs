module Pirec.Pretty.Context (Context, empty, extend) where

import Relude hiding (empty)

import Optics

import Pirec.Env (Env)
import Pirec.Env qualified as Env
import Pirec.Var.Name (Name (..))

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
