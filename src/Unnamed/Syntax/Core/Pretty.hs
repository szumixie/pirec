module Unnamed.Syntax.Core.Pretty (
  prettyTerm,
  prettyTermWith,
) where

import Relude hiding (group)

import Data.Char (isDigit)
import Data.Text.Read qualified as Text
import Data.Text.Short qualified as TS

import Optics
import Prettyprinter

import Unnamed.BoundMask qualified as BM
import Unnamed.Data.MultiMapAlter qualified as MMA
import Unnamed.Env qualified as Env
import Unnamed.Syntax.Core (Term (..))
import Unnamed.Syntax.Core.Pretty.Context (Context)
import Unnamed.Syntax.Core.Pretty.Context qualified as Ctx
import Unnamed.Var.Level (Level (..))
import Unnamed.Var.Name (Name (..))

prettyTerm :: Term -> Doc ann
prettyTerm = prettyTermWith Ctx.empty 0

prettyTermWith :: Context -> Int -> Term -> Doc ann
prettyTermWith !ctx = go
 where
  go !prec = \case
    Var lx@(Level n) ->
      ctx ^. #env & Env.index lx & maybe ("v" <> dot <> pretty n) pretty
    Meta mx Nothing -> pretty mx
    Meta mx (Just mask)
      | null args -> pretty mx
      | otherwise -> parensIf (prec > 10) $ pretty mx <+> align (sep args)
     where
      args = ctx ^. #env & toListOf (BM.masked mask % to pretty)
    t@Let{} -> prettyLet ctx t
    U -> "U"
    t@(Pi _ "_" _ _) -> parensIf (prec > 0) $ prettyFun ctx t
    t@Pi{} -> parensIf (prec > 0) $ prettyPi ctx t
    t@Lam{} -> parensIf (prec > 0) $ prettyLam ctx t
    t@App{} -> parensIf (prec > 10) $ u <+> align (sep args)
     where
      (u, args) = goApp [] t
    RowType a -> parensIf (prec > 10) $ "Row" <+> go 11 a
    RowLit ts ->
      ("#" <>) . align . encloseSep "{ " " }" ", " $
        itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go 0 t
    RowExt ts r ->
      ("#" <>) . align
        . encloseSep "{ " (line <> " | " <> go 0 r <> " }") ", "
        $ itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go 0 t
    RecordType r -> parensIf (prec > 10) $ "Rec" <+> go 11 r
    RecordLit ts ->
      ("rec" <>) . align . encloseSep "{ " " }" ", " $
        itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go 0 t
    RecordProj label index t ->
      foldr ($) (go 21 t) (replicate index (<> "." <> pretty label))
        <> dot
        <> pretty label
    RecordAlter ts u ->
      ("rec" <>) . align $
        encloseSep
          "{ "
          ( line <> " | "
              <> foldr (\label -> (<> ".-" <> pretty label)) (go 21 u) restrs
              <> " }"
          )
          ", "
          $ exts <&> \(x, t) -> pretty x <+> equals <+> go 0 t
     where
      (restrs, exts) =
        ts & itoListOf MMA.ifoldedAlter
          & partitionWith \(x, mt) -> case mt of
            Nothing -> Left x
            Just t -> Right (x, t)
  goApp args = \case
    App _ t u -> goApp (go 11 u : args) t
    t -> (go 11 t, args)

prettyLet :: Context -> Term -> Doc ann
prettyLet ctx t = "let" <> align (encloseSep "{ " " }" "; " $ go ctx t)
 where
  go ctx = \case
    Let (freshName (ctx ^. #names) -> x) t u ->
      pretty x <+> equals <+> prettyTermWith ctx 0 t : go (ctx & Ctx.extend x) u
    t -> [prettyTermWith ctx 0 t]

prettyPi :: Context -> Term -> Doc ann
prettyPi ctx t = align $ sep [forall_ <+> align (sep binders <+> arrow), u]
 where
  (binders, u) = go ctx t
  go ctx = \case
    Pi _ (freshName (ctx ^. #names) -> x) a b
      | x /= "_" ->
        go (ctx & Ctx.extend x) b
          & _1 %~ (parens (pretty x <+> colon <+> prettyTermWith ctx 0 a) :)
    t -> ([], prettyTermWith ctx 0 t)

prettyFun :: Context -> Term -> Doc ann
prettyFun ctx t = align (sep $ go ctx t)
 where
  go ctx = \case
    Pi _ "_" a b -> prettyTermWith ctx 1 a <+> arrow : go (ctx & Ctx.extend "_") b
    t -> [prettyTermWith ctx 0 t]

prettyLam :: Context -> Term -> Doc ann
prettyLam ctx t = align $ sep [lambda <+> align (sep binders <+> arrow), u]
 where
  (binders, u) = go ctx t
  go ctx = \case
    Lam _ (freshName (ctx ^. #names) -> x) t ->
      go (ctx & Ctx.extend x) t
        & _1 %~ (pretty x :)
    t -> ([], prettyTermWith ctx 0 t)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf b = if b then parens else id

freshName :: HashSet Name -> Name -> Name
freshName names name@(Name x)
  | name == "_" || not (names ^. contains name) = name
  | otherwise = go newIndex
 where
  (prefix, suffix) = TS.spanEnd isDigit x
  newIndex
    | TS.null suffix = 1
    | otherwise =
      suffix & TS.toText & Text.decimal @Int & fromRight (error "bug") & fst
  go !i
    | not $ names ^. contains newName = newName
    | otherwise = go $ i + 1
   where
    newName = Name $ prefix <> TS.fromString (show i)

forall_ :: Doc ann
forall_ = "∀"

arrow :: Doc ann
arrow = "→"

lambda :: Doc ann
lambda = "λ"
