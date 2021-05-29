module Pirec.Syntax.Raw.Parse.Type (Parser, run) where

import Relude

import Optics
import Text.Megaparsec

declareFieldLabels
  [d|
    newtype ParseContext = ParseContext
      { blockIndent :: Int
      }
      deriving stock (Show)
    |]

declareFieldLabels
  [d|
    data ParseState = ParseState
      { lexemeEnd :: Int
      , lineStart :: Bool
      }
      deriving stock (Show)
    |]

type Parser = ReaderT ParseContext (StateT ParseState (Parsec Void Text))

run :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
run p = parse do
  lexemeEnd <- getOffset
  p & usingReaderT (ParseContext 0)
    & evaluatingStateT (ParseState lexemeEnd True)
