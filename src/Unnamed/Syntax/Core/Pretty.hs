module Unnamed.Syntax.Core.Pretty (
  Context (..),
  emptyCtx,
  extendCtx,
  prettyTerm,
  prettyTermWith,
) where

import Relude

import Data.Char (isDigit)
import Data.Text.Read qualified as Text
import Data.Text.Short qualified as TS

import Optics
import Prettyprinter

import Unnamed.BoundMask qualified as BM
import Unnamed.Data.MultiMapAlter qualified as MMA
import Unnamed.Env (Env)
import Unnamed.Env qualified as Env
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Name (Name (..))

data Context = Context (Env Name) (HashSet Name)
  deriving stock (Show)

emptyCtx :: Context
emptyCtx = Context Env.empty mempty

extendCtx :: Name -> Context -> Context
extendCtx name (Context env names) =
  Context (env & Env.extend name) (names & contains name .~ True)

prettyTerm :: Term -> Doc ann
prettyTerm = prettyTermWith emptyCtx 0

prettyTermWith :: Context -> Int -> Term -> Doc ann
prettyTermWith ctx@(Context env names) = go
 where
  go !prec = \case
    Var lx@(Level n) -> env & Env.index lx & maybe ("@" <> pretty n) pretty
    Meta mx Nothing -> pretty mx
    Meta mx (Just mask) ->
      parensIf (prec > 10) $
        env & foldlOf' (BM.masked mask % to pretty) (<+>) (pretty mx)
    Let (freshName names -> x) t u ->
      parensIf (prec > 0) $
        hsep
          [ "let"
          , pretty x
          , equals
          , go 0 t
          , "in"
          , prettyTermWith (ctx & extendCtx x) 0 u
          ]
    U -> "U"
    Pi (freshName names -> x) a b
      | x == "_" ->
        parensIf (prec > 0) $
          go 1 a <+> "->" <+> prettyTermWith (ctx & extendCtx x) 0 b
      | otherwise ->
        parensIf (prec > 0) $
          parens (pretty x <+> colon <+> go 0 a) <+> "->"
            <+> prettyTermWith (ctx & extendCtx x) 0 b
    Lam (freshName names -> x) t ->
      parensIf (prec > 0) $
        backslash <> pretty x <> dot
          <+> prettyTermWith (ctx & extendCtx x) 0 t
    App t u -> parensIf (prec > 10) $ go 10 t <+> go 11 u
    RowType a -> parensIf (prec > 10) $ "Row" <+> go 11 a
    RowLit ts ->
      align . encloseSep "{ " " }" ", " $
        itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go 0 t
    RowExt ts r ->
      align
        ( encloseSep "{ " " | " ", " $
            itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go 0 t
        )
        <> go 0 r
        <> " }"
    RecordType r -> parensIf (prec > 10) $ "Record" <+> go 11 r
    RecordLit ts ->
      align . encloseSep "{ " " }" ", " $
        itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go 0 t
    RecordProj label index t ->
      foldr ($) (go 21 t) (replicate index (<> ".-" <> pretty label))
        <> dot
        <> pretty label
    RecordAlter ts u ->
      align
        ( encloseSep "{ " " | " ", " $
            exts <&> \(x, t) -> pretty x <+> equals <+> go 0 t
        )
        <> foldr (\label -> (<> ".-" <> pretty label)) (go 21 u) restrs
        <> " }"
     where
      (restrs, exts) =
        ts & itoListOf MMA.ifoldedAlter
          & partitionWith \(x, mt) -> case mt of
            Nothing -> Left x
            Just t -> Right (x, t)

freshName :: HashSet Name -> Name -> Name
freshName names name@(Name ts)
  | name == "_" || not (names ^. contains name) = name
  | otherwise = go newIndex
 where
  (prefix, suffix) = TS.spanEnd isDigit ts
  newIndex
    | TS.null suffix = 1
    | otherwise =
      suffix & TS.toText & Text.decimal @Int & fromRight (error "bug") & fst
  go !i
    | not $ names ^. contains newName = newName
    | otherwise = go $ i + 1
   where
    newName = Name $ prefix <> TS.fromString (show i)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf b = if b then parens else id
