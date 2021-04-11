module Unnamed.Syntax.Raw.Parse (Parser, parseRaw) where

import Relude hiding (many, some)

import Text.Megaparsec

import Unnamed.Data.Span
import Unnamed.Syntax.Raw qualified as R
import Unnamed.Syntax.Raw.Lex

data Level
  = LeftAssoc
      [Parser (R.Term -> R.Term -> R.Term)]
      [Parser (R.Term -> R.Term)]
      [Parser (R.Term -> R.Term)]
  | RightAssoc
      [Parser (R.Term -> R.Term -> R.Term)]
      [Parser (R.Term -> R.Term)]
      [Parser (R.Term -> R.Term)]

parseRaw :: String -> Text -> Either (ParseErrorBundle Text Void) R.Term
parseRaw = parse $ getOffset >>= evalStateT (spaceConsumer *> term <* eof)

term :: Parser R.Term
term = foldl' addPrec (parens term <|> withSpan (choice atoms)) ops

atoms :: [Parser R.Term]
atoms =
  [ termVar
  , termHole
  , termU
  , try termRowEmpty
  , try termRowLit
  , try termRecordEmpty
  , termRecordLit
  ]

ops :: [Level]
ops =
  [ LeftAssoc [] [] [try termRecordProj, termRecordRestr]
  , LeftAssoc [termApp] [termRowType, termRecordType] []
  , RightAssoc [termFun] [termLet, try termPi, termLam] []
  ]

withSpan :: Parser R.Term -> Parser R.Term
withSpan p = do
  start <- getOffset
  x <- p
  end <- get
  pure $ R.Span (Span start end) x

addPrec :: Parser R.Term -> Level -> Parser R.Term
addPrec p = \case
  LeftAssoc infl pf sf -> do
    start <- getOffset
    let go x =
          choice
            [ do
                f <- choice sf
                end <- get
                go $ R.Span (Span start end) (f x)
            , do
                f <- choice infl
                y <- p
                end <- get
                go $ R.Span (Span start end) (f x y)
            , pure x
            ]
    f <- option id $ choice pf
    x <- p
    go $ f x
  RightAssoc infr pf sf -> go
   where
    go = withSpan do
      choice pf <*> go <|> do
        x <- p
        choice
          [ choice infr ?? x <*> go
          , option id (choice sf) ?? x
          ]

termVar :: Parser R.Term
termVar = R.Var <$> ident

termHole :: Parser R.Term
termHole = R.Hole <$ uscore

termLet :: Parser (R.Term -> R.Term)
termLet =
  foldr (.) id <$ let_
    <*> (R.Let <$> ident <*> optional (colon *> term) <* equals <*> term)
      `sepEndBy1` semicolon <* in_

termU :: Parser R.Term
termU = R.U <$ univ

termPi :: Parser (R.Term -> R.Term)
termPi = foldr (.) id . join <$> some (parens binder) <* arrow
 where
  binder = do
    f <- some $ R.Pi <$> ident
    a <- colon *> term
    pure $ f ?? a

termFun :: Parser (R.Term -> R.Term -> R.Term)
termFun = R.Pi "_" <$ arrow

termLam :: Parser (R.Term -> R.Term)
termLam = foldr (.) id <$ lambda <*> some binder <* dot
 where
  binder =
    choice
      [ parens $ R.Lam <$> ident <* colon <*> (Just <$> term)
      , (`R.Lam` Nothing) <$> ident
      ]

termApp :: Parser (R.Term -> R.Term -> R.Term)
termApp = pure R.App

termRowType :: Parser (R.Term -> R.Term)
termRowType = R.RowType <$ row

termRowEmpty :: Parser R.Term
termRowEmpty = braces $ pure R.RowEmpty

termRowLit :: Parser R.Term
termRowLit = braces do
  exts <- (R.RowExt <$> ident <* colon <*> term) `sepEndBy1` comma
  rest <- option R.RowEmpty $ pipe *> term
  pure $ foldr ($) rest exts

termRecordType :: Parser (R.Term -> R.Term)
termRecordType = R.RecordType <$ record

termRecordEmpty :: Parser R.Term
termRecordEmpty = braces $ R.RecordEmpty <$ equals

termRecordLit :: Parser R.Term
termRecordLit = braces do
  exts <- (R.RecordExt <$> ident <* equals <*> term) `sepEndBy1` comma
  rest <- option R.RecordEmpty $ pipe *> term
  pure $ foldr ($) rest exts

termRecordProj :: Parser (R.Term -> R.Term)
termRecordProj = R.RecordProj <$ dot <*> ident

termRecordRestr :: Parser (R.Term -> R.Term)
termRecordRestr = R.RecordRestr <$ dot <* minus <*> ident
