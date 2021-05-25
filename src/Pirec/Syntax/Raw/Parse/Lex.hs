module Pirec.Syntax.Raw.Parse.Lex (
  space,
  ident,
  binderIdent,
  fieldLabel,
  fieldLabelOrName,
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
import Text.Megaparsec hiding (label)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

import Pirec.Label (Label, label)
import Pirec.Syntax.Raw.Parse.Type
import Pirec.Var.Name (Name (..), name)

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
    , "Type"
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

identText :: Parser Text
identText = lexeme $ try do
  offset <- getOffset
  text <- takeWhile1P (Just "identifier") isIdentLetter
  if keywords ^. contains text
    then
      region (setErrorOffset offset) . M.label "identifier" $
        unexpected (Tokens $ text & toString & fromList)
    else pure text

ident :: Parser Name
ident = name <$> identText

binderIdent :: Parser Name
binderIdent = ident <|> Wildcard <$ uscore

fieldLabel :: Parser Label
fieldLabel = fst <$> fieldLabelOrName

fieldLabelOrName :: Parser (Label, Maybe Name)
fieldLabelOrName =
  choice
    [ (label &&& Just . name) <$> identText
    , lexeme $
        (,Nothing) . label . fromString <$ single '"'
          <*> manyTill L.charLiteral (single '"')
    ]

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
univ = keyword "Type"
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
