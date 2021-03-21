module Unnamed.Syntax.Raw.Parse (Parser, parser) where

import Relude hiding (many, some)

import Data.Char (isLetter)
import Data.HashSet qualified as Set
import Data.List (foldl1')
import Data.List.NonEmpty qualified as NE

import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

import Unnamed.Syntax.Raw qualified as R
import Unnamed.Var.Name (Name, name)
import Unnamed.WithPos (WithPos (..))

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

keyword :: Text -> Parser Text
keyword kw = lexeme $ chunk kw <* notFollowedBy letterChar

keywords :: HashSet Text
keywords = Set.fromList ["let", "in", "U", "Row", "Record"]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

uscore, equals, semicolon, arrow, colon, lambda, dot, comma :: Parser Text
uscore = symbol "_"
equals = symbol "="
semicolon = symbol ";"
arrow = symbol "->"
colon = symbol ":"
lambda = symbol "\\"
dot = symbol "."
comma = symbol ","

let_, in_, univ, row, record :: Parser Text
let_ = keyword "let"
in_ = keyword "in"
univ = keyword "U"
row = keyword "Row"
record = keyword "Record"

ident :: Parser Name
ident = lexeme $ try do
  offset <- getOffset
  text <- takeWhile1P (Just "identifier") isLetter
  if text `Set.member` keywords
    then
      region (setErrorOffset offset) . label "identifier" $
        unexpected (Tokens $ text & toString & NE.fromList)
    else pure $ name text

parser :: Parser R.Term
parser = spaceConsumer *> term <* eof

term :: Parser R.Term
term = termPrec 0

termPrec :: Int -> Parser R.Term
termPrec prec
  | prec <= 0 = do
    pfs <- foldr (.) id <$> many (choice [termLet, try termPi, termLam])
    pos <- getSourcePos
    t <- termPrec 1
    option (pfs t) $ pfs . WithPos pos . R.Pi "_" t <$ arrow <*> termPrec 0
  | prec <= 10 = do
    pos <- getSourcePos
    choice
      [ WithPos pos <$> choice [termRowType, termRecordType]
      , some (termPrec 11) <&> foldl1' \t u -> WithPos pos $ R.App t u
      ]
  | prec <= 20 = do
    t <- termPrec 21
    flipfoldl' ($) t <$> many termRecordProj
  | otherwise =
    parens term
      <|> termWithPos
        (choice [termVar, termHole, termU, try termRowCon, termRecordCon])

termWithPos :: Parser R.Term' -> Parser R.Term
termWithPos p = WithPos <$> getSourcePos <*> p

termVar :: Parser R.Term'
termVar = R.Var <$> ident

termHole :: Parser R.Term'
termHole = R.Hole <$ uscore

termLet :: Parser (R.Term -> R.Term)
termLet = foldr (.) id <$ let_ <*> def `sepEndBy1` semicolon <* in_
 where
  def = do
    pos <- getSourcePos
    f <- R.Let <$> ident <* colon <*> term <* equals <*> term
    pure $ WithPos pos . f

termU :: Parser R.Term'
termU = R.U <$ univ

termPi :: Parser (R.Term -> R.Term)
termPi = foldr (.) id . join <$> some (parens binder) <* arrow
 where
  binder = do
    f <- some do
      pos <- getSourcePos
      f <- R.Pi <$> ident
      pure \a b -> WithPos pos $ f a b
    a <- colon *> term
    pure $ ($ a) <$> f

termLam :: Parser (R.Term -> R.Term)
termLam = foldr (.) id <$ lambda <*> some binder <* dot
 where
  binder = do
    pos <- getSourcePos
    f <- R.Lam <$> ident
    pure $ WithPos pos . f

termRowType :: Parser R.Term'
termRowType = R.RowType <$ row <*> termPrec 11

termRowCon :: Parser R.Term'
termRowCon =
  braces $ R.RowCon <$> ((,) <$> ident <* colon <*> term) `sepBy1` comma

termRecordType :: Parser R.Term'
termRecordType = R.RecordType <$ record <*> termPrec 11

termRecordCon :: Parser R.Term'
termRecordCon =
  braces $ R.RecordCon <$> ((,) <$> ident <* equals <*> term) `sepBy1` comma

termRecordProj :: Parser (R.Term -> R.Term)
termRecordProj = do
  pos <- getSourcePos
  f <- dot *> ident
  pure $ WithPos pos . R.RecordProj f
