module Pirec.Pretty (prettyTerm, prettyValue) where

import Relude hiding (group)
import Relude.Extra.Enum (next)

import Data.Char (isDigit)
import Data.Text.Read qualified as Text
import Data.Text.Short qualified as TS

import Control.Effect
import Optics
import Prettyprinter

import Pirec.BoundMask qualified as BM
import Pirec.Data.MultiMapAlter qualified as MMA
import Pirec.Elab.Context qualified as Elab
import Pirec.Env qualified as Env
import Pirec.Plicity (Plicity (..))
import Pirec.Pretty.Context (Context)
import Pirec.Pretty.Context qualified as Ctx
import Pirec.Syntax.Core (Term (..))
import Pirec.Syntax.Precedence (Precedence)
import Pirec.Syntax.Precedence qualified as P
import Pirec.Value (Value)
import Pirec.Var.Level (Level (..))
import Pirec.Var.Name (Name (..))

import Pirec.Effect.Meta (MetaLookup)
import Pirec.Eval (quote)

prettyTerm :: Term -> Doc ann
prettyTerm = prettyTermWith Ctx.empty minBound

prettyValue :: Eff MetaLookup m => Elab.Context -> Value -> m (Doc ann)
prettyValue ctx =
  fmap (prettyTermWith (Elab.prettyCtx ctx) minBound) . quote (Elab.level ctx)

prettyTermWith :: Context -> Precedence -> Term -> Doc ann
prettyTermWith !ctx = go
 where
  go !prec = \case
    Var lx@(Level n) ->
      ctx ^. #env & Env.index lx & maybe (dot <> pretty n) pretty
    Meta mx Nothing -> pretty mx
    Meta mx (Just mask)
      | null args -> pretty mx
      | otherwise -> parensPrec P.App $ pretty mx <+> align (sep args)
     where
      args = ctx ^. #env & toListOf (BM.masked mask % to pretty)
    t@Let{} -> prettyLet ctx t
    Univ -> "Type"
    t@(Pi Explicit Wildcard _ _) -> parensPrec P.Arrow $ prettyFun ctx t
    t@Pi{} -> parensPrec P.Arrow $ prettyPi ctx t
    t@Lam{} -> parensPrec P.Arrow $ prettyLam ctx t
    t@App{} -> parensPrec P.App $ u <+> align (sep args)
     where
      (u, args) = goApp [] t
    RowType a -> parensPrec P.App $ "Row" <+> go (next P.App) a
    RowLit ts ->
      ("#" <>) . align . encloseSep "{ " " }" ", " $
        itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go minBound t
    RowExt ts r ->
      ("#" <>) . align
        . encloseSep "{ " (line <> " | " <> go minBound r <> " }") ", "
        $ itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go minBound t
    RecordType r -> parensPrec P.App $ "Rec" <+> go (next P.App) r
    RecordLit ts ->
      ("rec" <>) . align . encloseSep "{ " " }" ", " $
        itoList ts <&> \((x, _), t) -> pretty x <+> colon <+> go minBound t
    RecordProj lbl index t ->
      parensPrec P.Proj $
        foldr ($) (go P.Proj t) (replicate index (<> "." <> pretty lbl))
          <> dot
          <> pretty lbl
    RecordAlter ts u
      | null exts -> parensPrec P.Proj restrsPretty
      | otherwise ->
        ("rec" <>) . align
          . encloseSep "{ " (line <> "| " <> restrsPretty <> " }") ", "
          $ exts <&> \(x, t) -> pretty x <+> equals <+> go minBound t
     where
      (restrs, exts) =
        ts & itoListOf MMA.ifoldedAlter
          & partitionWith \(x, mt) -> case mt of
            Nothing -> Left x
            Just t -> Right (x, t)
      restrsPretty
        | null restrs = go minBound u
        | otherwise =
          foldr (\lbl -> (<> ".-" <> pretty lbl)) (go P.Proj u) restrs
   where
    parensPrec prec' = if prec > prec' then parens else id
  goApp args = \case
    App pl t u -> goApp (arg : args) t
     where
      arg = case pl of
        Explicit -> go (next P.App) u
        Implicit -> braces (go minBound u)
    t -> (go (next P.App) t, args)

prettyLet :: Context -> Term -> Doc ann
prettyLet ctx t = "let" <> align (encloseSep "{ " " }" "; " $ go ctx t)
 where
  go ctx = \case
    Let (freshName (ctx ^. #names) -> x) t u ->
      pretty x <+> equals <+> prettyTermWith ctx minBound t :
      go (ctx & Ctx.extend x) u
    t -> [prettyTermWith ctx minBound t]

prettyPi :: Context -> Term -> Doc ann
prettyPi ctx t = align $ sep [forall_ <+> align (sep binders <+> arrow), u]
 where
  (binders, u) = go ctx t
  go ctx = \case
    Pi pl (freshName (ctx ^. #names) -> x) a b
      | pl == Implicit || x /= Wildcard ->
        go (ctx & Ctx.extend x) b
          & _1
            %~ (encl (pretty x <+> colon <+> prettyTermWith ctx minBound a) :)
     where
      encl = case pl of
        Explicit -> parens
        Implicit -> braces
    t -> ([], prettyTermWith ctx P.Arrow t)

prettyFun :: Context -> Term -> Doc ann
prettyFun ctx t = align (sep $ go ctx t)
 where
  go ctx = \case
    Pi Explicit Wildcard a b ->
      prettyTermWith ctx (next P.Arrow) a <+> arrow :
      go (ctx & Ctx.extend Wildcard) b
    t -> [prettyTermWith ctx P.Arrow t]

prettyLam :: Context -> Term -> Doc ann
prettyLam ctx t = align $ sep [lambda <+> align (sep binders <+> arrow), u]
 where
  (binders, u) = go ctx t
  go ctx = \case
    Lam pl (freshName (ctx ^. #names) -> x) t ->
      go (ctx & Ctx.extend x) t & _1 %~ (encl (pretty x) :)
     where
      encl = case pl of
        Explicit -> id
        Implicit -> braces
    t -> ([], prettyTermWith ctx P.Arrow t)

freshName :: HashSet Name -> Name -> Name
freshName names name@(Name x)
  | name == Wildcard || not (names ^. contains name) = name
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
