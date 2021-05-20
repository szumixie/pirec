module Pirec.Value (
  Closure (..),
  Value (..),
  Var (..),
  Spine (..),
  var,
  meta,
  rowExt,
  recordProj,
  recordAlter,
) where

import Relude

import Optics

import Pirec.Data.MultiMap (MultiMap)
import Pirec.Data.MultiMapAlter (MultiMapAlter)
import Pirec.Data.MultiMapAlter qualified as MMA
import Pirec.Env (Env)
import Pirec.Plicity (Plicity)
import Pirec.Syntax.Core (Term)
import Pirec.Var.Level (Level)
import Pirec.Var.Meta (Meta)
import Pirec.Var.Name (Name)

data Closure = Closure (Env Value) Term
  deriving stock (Show)

data Value
  = Neut Var Spine
  | Univ
  | Pi Plicity Name ~Value Closure
  | Lam Plicity Name Closure
  | RowType Value
  | RowLit (MultiMap Name Value)
  | RecordType Value
  | RecordLit (MultiMap Name Value)
  deriving stock (Show)

data Var
  = Rigid Level
  | Flex Meta
  deriving stock (Show, Eq)

data Spine
  = Nil
  | App Plicity Spine ~Value
  | RowExt (MultiMap Name Value) Spine
  | RecordProj Name Int Spine
  | RecordAlter (MultiMapAlter Name Value) Spine
  deriving stock (Show)

var :: Level -> Value
var lx = Neut (Rigid lx) Nil

meta :: Meta -> Value
meta mx = Neut (Flex mx) Nil

rowExt :: MultiMap Name Value -> Value -> Value
rowExt ts = \case
  Neut x spine -> Neut x case spine of
    RowExt us spine -> RowExt (ts <> us) spine
    _ -> RowExt ts spine
  RowLit us -> RowLit (ts <> us)
  _ -> error "bug"

recordProj :: Name -> Int -> Value -> Value
recordProj label index = \case
  Neut x spine -> case spine of
    RecordAlter ts spine -> case ts & MMA.lookup label index of
      Left index -> Neut x $ RecordProj label index spine
      Right t -> t
    _ -> Neut x $ RecordProj label index spine
  RecordLit ts -> ts ^? ix (label, index) ?: error "bug"
  _ -> error "bug"

recordAlter :: MultiMapAlter Name Value -> Value -> Value
recordAlter ts = \case
  Neut x spine -> Neut x case spine of
    RecordAlter us spine ->
      maybe id RecordAlter (guarded (not . null) (ts <> us)) spine
    _ -> RecordAlter ts spine
  RecordLit us -> RecordLit (MMA.apply ts us)
  _ -> error "bug"
