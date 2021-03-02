module Unnamed.Value (
  Closure (..),
  Value (..),
  Neutral (..),
  var,
  app,
) where

import Data.HashMap.Strict (HashMap)

import Optics (declareFieldLabels)

import Unnamed.Env (Env)
import Unnamed.Syntax.Core (Term)
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Name (Name)

declareFieldLabels
  [d|
    data Closure = Closure {env :: Env Value, body :: Term}
      deriving stock (Show)

    data Value
      = Neut {neut :: Neutral}
      | U
      | Pi
          { name :: {-# UNPACK #-} Name
          , source :: ~Value
          , target :: {-# UNPACK #-} Closure
          }
      | Lam
          { name :: {-# UNPACK #-} Name
          , body :: {-# UNPACK #-} Closure
          }
      | Row {typ :: Value}
      | RowCon {elems :: HashMap Name Value}
      | Record {row :: Value}
      | RecordCon {elems :: HashMap Name Value}
      deriving stock (Show)

    data Neutral
      = Var {level :: {-# UNPACK #-} Level}
      | App {fun :: Neutral, arg :: ~Value}
      | RecordProj {field :: {-# UNPACK #-} Name, record :: Neutral}
      deriving stock (Show)
    |]

var :: Level -> Value
var = Neut . Var

app :: Neutral -> Value -> Value
app t u = Neut $ App t u
