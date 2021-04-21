module Unnamed.Syntax.Raw.Parse.Type (ParseState, Parser, run) where

import Relude

import Optics
import Text.Megaparsec

declareFieldLabels
  [d|
    newtype ParseState = ParseState
      { lexemeEnd :: Int
      }
    |]

type Parser = StateT ParseState (Parsec Void Text)

run :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
run p = parse do
  lexemeEnd <- getOffset
  evalStateT p (ParseState lexemeEnd)
