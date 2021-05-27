module Pirec.Value (
  Closure (..),
  Value (..),
  Var (..),
  Spine (..),
  var,
  meta,
  proj1,
  proj2,
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
import Pirec.Label (Label)
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
  | Sigma Name ~Value Closure
  | Pair Value ~Value
  | RowType Value
  | RowLit (MultiMap Label Value)
  | RecordType Value
  | RecordLit (MultiMap Label Value)
  deriving stock (Show)

data Var
  = Rigid Level
  | Flex Meta
  deriving stock (Show, Eq)

data Spine
  = Nil
  | App Plicity Spine ~Value
  | Proj1 Spine
  | Proj2 Spine
  | RowExt (MultiMap Label Value) Spine
  | RecordProj Label Int Spine
  | RecordAlter (MultiMapAlter Label Value) Spine
  deriving stock (Show)

var :: Level -> Value
var lx = Neut (Rigid lx) Nil

meta :: Meta -> Value
meta mx = Neut (Flex mx) Nil

proj1 :: Value -> Value
proj1 = \case
  Neut x spine -> Neut x $ Proj1 spine
  Pair t _ -> t
  _ -> error "bug"

proj2 :: Value -> Value
proj2 = \case
  Neut x spine -> Neut x $ Proj2 spine
  Pair _ u -> u
  _ -> error "bug"

rowExt :: MultiMap Label Value -> Value -> Value
rowExt ts = \case
  Neut x spine -> Neut x case spine of
    RowExt us spine -> RowExt (ts <> us) spine
    _ -> RowExt ts spine
  RowLit us -> RowLit (ts <> us)
  _ -> error "bug"

recordProj :: Label -> Int -> Value -> Value
recordProj lbl index = \case
  Neut x spine -> case spine of
    RecordAlter ts spine -> case ts & MMA.lookup lbl index of
      Left index -> Neut x $ RecordProj lbl index spine
      Right t -> t
    _ -> Neut x $ RecordProj lbl index spine
  RecordLit ts -> ts ^? ix (lbl, index) ?: error "bug"
  _ -> error "bug"

recordAlter :: MultiMapAlter Label Value -> Value -> Value
recordAlter ts = \case
  Neut x spine -> Neut x case spine of
    RecordAlter us spine ->
      maybe id RecordAlter (guarded (not . null) (ts <> us)) spine
    _ -> RecordAlter ts spine
  RecordLit us -> RecordLit (MMA.apply ts us)
  _ -> error "bug"
