module Unnamed.Syntax.Core.Pretty (prettyTerm, prettyTermWith) where

import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Text.Read qualified as Text
import Data.Text.Short qualified as TS

import Prettyprinter

import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Var.Name (Name (..))

prettyTerm :: Term -> Doc ann
prettyTerm = prettyTermWith Env.empty Set.empty 0

prettyTermWith :: Env Name -> HashSet Name -> Int -> Term -> Doc ann
prettyTermWith !env !names = go
 where
  go !prec = \case
    Var x -> Env.index x env & fromMaybe (error "bug") & prettyName
    Let (freshName names -> x) a t u ->
      parensIf (prec > 0) $
        hsep
          [ "let"
          , prettyName x
          , colon
          , go 0 a
          , equals
          , go 0 t
          , "in"
          , prettyTermWith (Env.extend x env) (Set.insert x names) 0 u
          ]
    U -> "U"
    Pi (freshName names -> x) a b
      | x == "_" -> parensIf (prec > 0) $ go 1 a <+> "->" <+> go 0 b
      | otherwise ->
        parensIf (prec > 0) $
          parens (prettyName x <+> colon <+> go 0 a) <+> "->"
            <+> prettyTermWith (Env.extend x env) (Set.insert x names) 0 b
    Lam (freshName names -> x) t ->
      parensIf (prec > 0) $
        backslash <> prettyName x <> dot
          <+> prettyTermWith (Env.extend x env) (Set.insert x names) 0 t
    App t u -> parensIf (prec > 10) $ go 10 t <+> go 11 u

prettyName :: Name -> Doc ann
prettyName (Name ts) = pretty $ TS.toText ts

freshName :: HashSet Name -> Name -> Name
freshName names name@(Name ts)
  | name == "_" || not (name `Set.member` names) = name
  | otherwise = go newIndex
 where
  (prefix, suffix) = TS.spanEnd isDigit ts
  newIndex
    | TS.null suffix = 1
    | otherwise =
      suffix & TS.toText & Text.decimal @Int & fromRight (error "bug") & fst
  go !i
    | not $ newName `Set.member` names = newName
    | otherwise = go $ i + 1
   where
    newName = Name $ prefix <> TS.fromString (show i)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf b = if b then parens else id
