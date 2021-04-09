module Unnamed.Value (
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

import Unnamed.Data.MultiMap (MultiMap)
import Unnamed.Data.MultiMapAlter (MultiMapAlter)
import Unnamed.Data.MultiMapAlter qualified as MMA
import Unnamed.Env (Env)
import Unnamed.Syntax.Core (Term)
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Meta (Meta)
import Unnamed.Var.Name (Name)

data Closure = Closure (Env Value) Term
  deriving stock (Show)

data Value
  = Neut Var Spine
  | U
  | Pi {-# UNPACK #-} Name ~Value Closure
  | Lam {-# UNPACK #-} Name {-# UNPACK #-} Closure
  | RowType Value
  | RowLit (MultiMap Name Value)
  | RecordType Value
  | RecordLit (MultiMap Name Value)
  deriving stock (Show)

data Var
  = Rigid {-# UNPACK #-} Level
  | Flex {-# UNPACK #-} Meta
  deriving stock (Show, Eq)

data Spine
  = Nil
  | App Spine ~Value
  | RowExt (MultiMap Name Value) Spine
  | RecordProj {-# UNPACK #-} Name {-# UNPACK #-} Int Spine
  | RecordAlter (MultiMapAlter Name Value) Spine
  deriving stock (Show)

var :: Level -> Value
var lx = Neut (Rigid lx) Nil

meta :: Meta -> Value
meta mx = Neut (Flex mx) Nil

rowExt :: MultiMap Name Value -> Value -> Value
rowExt vts = \case
  Neut x spine0 -> Neut x case spine0 of
    RowExt vus spine -> RowExt (vts <> vus) spine
    spine -> RowExt vts spine
  RowLit vus -> RowLit (vts <> vus)
  _ -> error "bug"

recordProj :: Name -> Int -> Value -> Value
recordProj label index = \case
  Neut x spine0 -> case spine0 of
    RecordAlter vts spine -> case vts & MMA.lookup label index of
      Left index' -> Neut x $ RecordProj label index' spine
      Right vt -> vt
    spine -> Neut x $ RecordProj label index spine
  RecordLit vts -> vts ^? ix (label, index) ?: error "bug"
  _ -> error "bug"

recordAlter :: MultiMapAlter Name Value -> Value -> Value
recordAlter vts = \case
  Neut x spine0 -> Neut x case spine0 of
    RecordAlter vus spine ->
      maybe id RecordAlter (guarded (not . null) (vts <> vus)) spine
    spine -> RecordAlter vts spine
  RecordLit vus -> RecordLit (MMA.apply vts vus)
  _ -> error "bug"
