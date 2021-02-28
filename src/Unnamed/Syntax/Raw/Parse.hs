module Unnamed.Syntax.Raw.Parse (Parser, parser) where

import Data.Char (isLetter)
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)

import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

import Unnamed.Syntax.Raw qualified as R
import Unnamed.Var.Name (name)
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
keywords = Set.fromList ["let", "in", "U"]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

let_, equals, semicolon, in_, univ, arrow, colon, lambda, dot :: Parser Text
let_ = keyword "let"
equals = symbol "="
semicolon = symbol ";"
in_ = keyword "in"
univ = keyword "U"
arrow = symbol "->"
colon = symbol ":"
lambda = symbol "\\"
dot = symbol "."

ident :: Parser Text
ident = lexeme $ try do
  offset <- getOffset
  text <- takeWhile1P (Just "identifier") isLetter
  if Set.member text keywords
    then
      region (setErrorOffset offset) . label "identifier" $
        unexpected (Tokens . NE.fromList . Text.unpack $ text)
    else pure text

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
    some (termPrec 11) <&> foldl1' \t u -> WithPos pos $ R.App t u
  | otherwise =
    parens term <|> WithPos <$> getSourcePos <*> choice [termVar, termU]

termVar :: Parser R.Term'
termVar = R.Var . name <$> ident

termLet :: Parser (R.Term -> R.Term)
termLet = foldr (.) id <$ let_ <*> def `sepEndBy1` semicolon <* in_
 where
  def = do
    pos <- getSourcePos
    f <- R.Let <$> (name <$> ident) <* colon <*> term <* equals <*> term
    pure $ WithPos pos . f

termU :: Parser R.Term'
termU = R.U <$ univ

termPi :: Parser (R.Term -> R.Term)
termPi = foldr (.) id . mconcat <$> some (parens binder) <* arrow
 where
  binder = do
    f <- some do
      pos <- getSourcePos
      f <- R.Pi <$> (name <$> ident)
      pure \a b -> WithPos pos $ f a b
    a <- colon *> term
    pure $ ($ a) <$> f

termLam :: Parser (R.Term -> R.Term)
termLam = foldr (.) id <$ lambda <*> some binder <* dot
 where
  binder = do
    pos <- getSourcePos
    f <- R.Lam <$> (name <$> ident)
    pure $ WithPos pos . f
