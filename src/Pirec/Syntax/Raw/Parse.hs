module Pirec.Syntax.Raw.Parse (parseRaw) where

import Relude hiding (many, some)
import Relude.Extra.Enum (next)

import Optics
import Text.Megaparsec hiding (State)

import Pirec.Data.Span
import Pirec.Plicity (Plicity (..))
import Pirec.Syntax.Precedence (Precedence)
import Pirec.Syntax.Precedence qualified as P
import Pirec.Syntax.Raw qualified as R
import Pirec.Syntax.Raw.Parse.Lex
import Pirec.Syntax.Raw.Parse.Type
import Pirec.Var.Name (Name)

parseRaw :: String -> Text -> Either (ParseErrorBundle Text Void) R.Term
parseRaw = run $ space *> termLetBlock <* eof

term :: Parser R.Term
term = termPrec minBound

termPrec :: Precedence -> Parser R.Term
termPrec !prec = label "expression" case prec of
  P.Atom ->
    choice
      [ parens term
      , withSpan $
          choice
            [ R.Var <$> ident
            , R.Hole <$ uscore
            , termLet
            , R.Univ <$ univ
            , termPi
            , termLam
            , termRowLit
            , termRecordLit
            ]
      ]
  P.Proj ->
    suffixes (termPrec $ next prec) . label "record projection" $
      choice
        [ R.RecordRestr <$ dotminus <*> fieldLabel
        , R.RecordProj <$ dot <*> fieldLabel
        ]
  P.App -> suffixes fun do
    (pl, u) <-
      braces ((Implicit,) <$> term) <|> (Explicit,) <$> termPrec (next prec)
    pure \t -> R.App pl t u
   where
    fun =
      choice
        [ withSpan $
            choice
              [ R.RowType <$ rowType <*> termPrec (next prec)
              , R.RecordType <$ recordType <*> termPrec (next prec)
              ]
        , termPrec (next prec)
        ]
  P.Arrow -> go
   where
    go = withSpan do
      t <- termPrec (next prec)
      option t $ R.Pi Explicit "_" (Just t) <$ arrow <*> go

termLet :: Parser R.Term
termLet = let_ *> termLetBlock

termLetBlock :: Parser R.Term
termLetBlock = indentBlock \sep -> do
  fs <- many do
    (x, hasType) <- try $ (,) <$> ident <*> (True <$ colon <|> False <$ equals)
    a <- if hasType then Just <$> term <* equals else pure Nothing
    R.Let x a <$> term <* sep
  foldr (.) id fs <$> term

termPi :: Parser R.Term
termPi =
  foldr (.) id <$ forall_
    <*> some (binder <&> \(pl, x, a) -> R.Pi pl x a) <* arrow
    <*> termPrec P.Arrow

termLam :: Parser R.Term
termLam =
  foldr (.) id <$ lambda
    <*> some (binder <&> \(pl, x, a) -> R.Lam pl x a) <* arrow
    <*> termPrec P.Arrow

termRowLit :: Parser R.Term
termRowLit =
  rowLit *> rowBlock R.RowEmpty (R.RowExt <$> fieldLabel <* colon <*> term)

termRecordLit :: Parser R.Term
termRecordLit =
  recordLit *> rowBlock R.RecordEmpty do
    label <- fieldLabel
    let extend a = R.RecordExt label a <$ equals <*> term
        modify a = do
          t <- coloneq *> term
          pure $ R.RecordExt label a t . R.RecordRestr label
    choice
      [ extend Nothing <|> modify Nothing
      , do
          a <- colon *> term
          extend (Just a) <|> modify (Just a)
      , pure $ R.RecordExt label Nothing (R.Var label)
      ]

withSpan :: Parser R.Term -> Parser R.Term
withSpan p = do
  start <- getOffset
  x <- p
  end <- use #lexemeEnd
  pure $ R.Span (Span start end) x

suffixes :: Parser R.Term -> Parser (R.Term -> R.Term) -> Parser R.Term
suffixes p sf = do
  start <- getOffset
  let go t = option t do
        f <- sf
        end <- use #lexemeEnd
        go $ R.Span (Span start end) (f t)
  t <- p
  go t

binder :: Parser (Plicity, Name, Maybe R.Term)
binder =
  choice
    [ parens $ (Explicit,,) <$> binderIdent <*> optional (colon *> term)
    , braces $ (Implicit,,) <$> binderIdent <*> optional (colon *> term)
    , (Explicit,,Nothing) <$> binderIdent
    ]
    <?> "binder"

rowBlock :: R.Term -> Parser (R.Term -> R.Term) -> Parser R.Term
rowBlock empty field = indentBlock \sep -> do
  fs <- field `sepBy` sep
  r <-
    if null fs then pure empty else option empty $ optional sep *> pipe *> term
  pure $ foldr ($) r fs
