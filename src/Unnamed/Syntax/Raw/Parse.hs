module Unnamed.Syntax.Raw.Parse (parseRaw) where

import Relude hiding (many, some)

import Optics
import Text.Megaparsec hiding (State)

import Unnamed.Data.Span
import Unnamed.Plicity (Plicity (..))
import Unnamed.Syntax.Raw qualified as R
import Unnamed.Syntax.Raw.Parse.Lex
import Unnamed.Syntax.Raw.Parse.Type
import Unnamed.Var.Name (Name)

parseRaw :: String -> Text -> Either (ParseErrorBundle Text Void) R.Term
parseRaw = run $ space *> termLetBlock <* eof

term :: Parser R.Term
term = atom & termRecordSuffix & termApp & termFun

atom :: Parser R.Term
atom =
  choice
    [ parens term
    , withSpan $
        choice
          [ R.Var <$> ident
          , R.Hole <$ uscore
          , termLet
          , R.U <$ univ
          , termPi
          , termLam
          , termRowLit
          , termRecordLit
          ]
    ]
    <?> "expression"

termFun :: Parser R.Term -> Parser R.Term
termFun p = go
 where
  go = withSpan do
    t <- p
    option t $ R.Pi Explicit "_" (Just t) <$ arrow <*> go

termApp :: Parser R.Term -> Parser R.Term
termApp p = suffixes fun $ do
  (pl, u) <- braces ((Implicit,) <$> term) <|> (Explicit,) <$> p
  pure \t -> R.App pl t u
 where
  fun =
    choice
      [ withSpan . hidden $
          R.RowType <$ rowType <*> p <|> R.RecordType <$ recordType <*> p
      , p
      ]

termRecordSuffix :: Parser R.Term -> Parser R.Term
termRecordSuffix p =
  suffixes p . label "record projection" $
    R.RecordRestr <$ dotminus <*> fieldLabel
      <|> R.RecordProj <$ dot <*> fieldLabel

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
    <*> term

termLam :: Parser R.Term
termLam =
  foldr (.) id <$ lambda
    <*> some (binder <&> \(pl, x, a) -> R.Lam pl x a) <* arrow
    <*> term

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
