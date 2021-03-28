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
  , try termRowLit
  , try termRowCons
  , try termEmptyRecordCon
  , termRecordCon
  ]

ops :: [Level]
ops =
  [ LeftAssoc [] [] [termRecordProj]
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
  RightAssoc infr pf sf ->
    let go = withSpan do
          choice pf <*> go <|> do
            x <- p
            choice
              [ choice infr ?? x <*> go
              , option id (choice sf) ?? x
              ]
     in go

termVar :: Parser R.Term
termVar = R.Var <$> ident

termHole :: Parser R.Term
termHole = R.Hole <$ uscore

termLet :: Parser (R.Term -> R.Term)
termLet =
  foldr (.) id <$ let_
    <*> (R.Let <$> ident <* colon <*> term <* equals <*> term)
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
termLam = foldr (.) id <$ lambda <*> some (R.Lam <$> ident) <* dot

termApp :: Parser (R.Term -> R.Term -> R.Term)
termApp = pure R.App

termRowType :: Parser (R.Term -> R.Term)
termRowType = R.RowType <$ row <* diff <*> braces (many ident)

termRowLit :: Parser R.Term
termRowLit =
  braces $ R.RowLit <$> ((,) <$> ident <* colon <*> term) `sepEndBy` comma

termRowCons :: Parser R.Term
termRowCons =
  braces $
    R.RowCons <$> ((,) <$> ident <* colon <*> term) `sepEndBy` comma <* pipe
      <*> term

termRecordType :: Parser (R.Term -> R.Term)
termRecordType = R.RecordType <$ record

termRecordCon :: Parser R.Term
termRecordCon =
  braces $ R.RecordLit <$> ((,) <$> ident <* equals <*> term) `sepEndBy1` comma

termEmptyRecordCon :: Parser R.Term
termEmptyRecordCon = R.RecordLit [] <$ braces equals

termRecordProj :: Parser (R.Term -> R.Term)
termRecordProj = R.RecordProj <$ dot <*> ident
