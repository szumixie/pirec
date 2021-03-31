module Unnamed.Syntax.Raw.Lex (
  Parser,
  spaceConsumer,
  lexeme,
  symbol,
  keyword,
  keywords,
  ident,
  parens,
  braces,
  uscore,
  equals,
  semicolon,
  arrow,
  colon,
  lambda,
  dot,
  diff,
  comma,
  pipe,
  let_,
  in_,
  univ,
  row,
  record,
) where

import Relude hiding (many, some)

import Data.Char (isAlphaNum)
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE

import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

import Unnamed.Var.Name (Name, name)

type Parser = StateT Int (Parsec Void Text)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  put =<< getOffset
  x <$ spaceConsumer

symbol :: Text -> Parser Text
symbol = lexeme . chunk

keyword :: Text -> Parser Text
keyword kw = lexeme $ chunk kw <* notFollowedBy alphaNumChar

keywords :: HashSet Text
keywords = Set.fromList ["let", "in", "U", "Row", "Record"]

ident :: Parser Name
ident = lexeme $ try do
  offset <- getOffset
  text <- takeWhile1P (Just "identifier") isAlphaNum
  if text `Set.member` keywords
    then
      region (setErrorOffset offset) . label "identifier" $
        unexpected (Tokens $ text & toString & NE.fromList)
    else pure $ name text

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

uscore
  , equals
  , semicolon
  , arrow
  , colon
  , lambda
  , dot
  , diff
  , comma
  , pipe ::
    Parser Text
uscore = symbol "_"
equals = symbol "="
semicolon = symbol ";"
arrow = symbol "->"
colon = symbol ":"
lambda = symbol "\\"
dot = symbol "."
diff = symbol "\\"
comma = symbol ","
pipe = symbol "|"

let_, in_, univ, row, record :: Parser Text
let_ = keyword "let"
in_ = keyword "in"
univ = keyword "U"
row = keyword "Row"
record = keyword "Record"
