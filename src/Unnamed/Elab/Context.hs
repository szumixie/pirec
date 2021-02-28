module Unnamed.Elab.Context (
  Context,
  empty,
  extend,
  bind,
  level,
  env,
  getType,
  getNameSet,
  getNameEnv,
) where

import Control.Category ((>>>))
import Data.Foldable (foldl')
import Data.Functor (void)
import GHC.Exts (sortWith)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set

import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Value (Value)
import Unnamed.Value qualified as V
import Unnamed.Var.Level (Level)
import Unnamed.Var.Name (Name)

data Context = Context
  { contextLevel :: {-# UNPACK #-} Level
  , contextTypes :: HashMap Name (Level, Value)
  , contextEnv :: Env Value
  }
  deriving stock (Show)

empty :: Context
empty = Context 0 Map.empty Env.empty

extend :: Name -> Value -> Value -> Context -> Context
extend x a t (Context lvl types env') =
  Context (lvl + 1) (Map.insert x (lvl, a) types) (Env.extend t env')

bind :: Name -> Value -> Context -> Context
bind x a ctx = extend x a (V.var $ level ctx) ctx

level :: Context -> Level
level = contextLevel

env :: Context -> Env Value
env = contextEnv

getType :: Name -> Context -> Maybe (Level, Value)
getType x = Map.lookup x . contextTypes

getNameSet :: Context -> HashSet Name
getNameSet = Set.fromMap . void . contextTypes

getNameEnv :: Context -> Env Name
getNameEnv =
  contextTypes
    >>> Map.toList
    >>> fmap (fmap fst)
    >>> sortWith snd
    >>> fmap fst
    >>> foldl' (flip Env.extend) Env.empty
