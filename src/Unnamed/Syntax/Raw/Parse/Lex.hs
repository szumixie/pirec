module Unnamed.Syntax.Raw.Parse.Lex (
  space,
  ident,
  binderIdent,
  fieldLabel,
  indentBlock,
  parens,
  braces,
  semicolon,
  uscore,
  let_,
  equals,
  colon,
  univ,
  arrow,
  forall_,
  lambda,
  rowType,
  rowLit,
  pipe,
  recordType,
  recordLit,
  dot,
  dotminus,
  coloneq,
) where

import Relude hiding (many, some)

import Data.Char qualified as C
import Data.HashSet qualified as HSet
import Data.IntSet qualified as ISet

import Optics
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

import Unnamed.Syntax.Raw.Parse.Type
import Unnamed.Var.Name (Name, name)

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = do
  _ <- chunk "{-"
  skipManyTill (blockComment <|> void anySingle) (void (chunk "-}") <|> eof)

space :: Parser ()
space = L.space C.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme p = do
  lineStart <- use #lineStart
  _ <- L.indentGuard pass (if lineStart then EQ else GT) =<< gview #blockIndent
  x <- p
  assign #lexemeEnd =<< getOffset
  assign #lineStart False
  x <$ space

symbol :: Text -> Parser Text
symbol = lexeme . chunk

specialChars :: IntSet
specialChars = ISet.fromList $ fromEnum <$> "(){};\\.\""

isIdentLetter :: Char -> Bool
isIdentLetter c
  | C.isAlphaNum c || C.isMark c = True
  | specialChars ^. contains (fromEnum c) = False
  | otherwise = C.isPunctuation c || C.isSymbol c

keywords :: HashSet Text
keywords =
  HSet.fromList
    [ "_"
    , "let"
    , "="
    , ":"
    , "U"
    , "→"
    , "->"
    , "∀"
    , "forall"
    , "λ"
    , "Row"
    , "#"
    , "|"
    , "Rec"
    , "rec"
    , ":="
    ]

keyword :: Text -> Parser Text
keyword kw = lexeme $ chunk kw <* notFollowedBy (satisfy isIdentLetter)

ident :: Parser Name
ident = lexeme $ try do
  offset <- getOffset
  text <- takeWhile1P (Just "identifier") isIdentLetter
  if keywords ^. contains text
    then
      region (setErrorOffset offset) . label "identifier" $
        unexpected (Tokens $ text & toString & fromList)
    else pure $ name text

binderIdent :: Parser Name
binderIdent = ident <|> name <$> uscore

fieldLabel :: Parser Name
fieldLabel =
  choice
    [ ident
    , lexeme $
        name . fromString <$ single '"' <*> manyTill L.charLiteral (single '"')
    ]
    <?> "field label"

indentBlock :: (Parser () -> Parser a) -> Parser a
indentBlock f =
  choice
    [ braces $ f (void semicolon) <* optional semicolon
    , do
        blockIndent <- L.indentLevel
        local (#blockIndent .~ blockIndent) $
          assign #lineStart True *> f (assign #lineStart True)
    ]
    <?> "indented block"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semicolon
  , uscore
  , let_
  , equals
  , colon
  , univ
  , arrow
  , forall_
  , lambda
  , rowType
  , rowLit
  , pipe
  , recordType
  , recordLit
  , dot
  , dotminus
  , coloneq ::
    Parser Text
semicolon = symbol ";"
uscore = keyword "_"
let_ = keyword "let"
equals = keyword "="
colon = keyword ":"
univ = keyword "U"
arrow = keyword "→" <|> keyword "->" <?> "arrow"
forall_ = keyword "∀" <|> keyword "forall" <?> "forall"
lambda = keyword "λ" <|> symbol "\\" <?> "lambda"
rowType = keyword "Row"
rowLit = keyword "#"
pipe = keyword "|"
recordType = keyword "Rec"
recordLit = keyword "rec"
dot = symbol "."
dotminus = symbol ".-"
coloneq = keyword ":="
