module Pirec.Options (Options (..), Input (..), defaultOpts, getOpts) where

import Relude

import Data.Version (showVersion)

import Control.Applicative.Combinators (choice)
import Optics
import Options.Applicative

import Paths_pirec qualified as Paths

data Input
  = File FilePath
  | Stdin
  deriving stock (Show)

declareFieldLabels
  [d|
    data Options = Options
      { input :: Input
      , eval :: Bool
      , showTerm :: Bool
      , showType :: Bool
      , showMetas :: Bool
      }
      deriving stock (Show)
    |]

defaultOpts :: Input -> Options
defaultOpts input = Options input True True True False

getOpts :: IO Options
getOpts =
  execParser . info (options <**> version <**> helper) $
    header "Pirec type checker and interpreter"

options :: Parser Options
options = Options <$> input <*> eval <*> showTerm <*> showType <*> showMetas

input :: Parser Input
input =
  choice
    [ File <$> strArgument (metavar "FILEPATH" <> action "file")
    , flag' Stdin $
        mconcat
          [ long "stdin"
          , help "Read the input from stdin"
          ]
    ]

eval :: Parser Bool
eval =
  fmap not . switch $
    mconcat
      [ long "elab-only"
      , help "Elaborate the input without evaluating to normal form"
      ]

showTerm :: Parser Bool
showTerm =
  fmap not . switch $
    mconcat
      [ long "no-term"
      , help "Do not show the elaborated term"
      ]

showType :: Parser Bool
showType =
  fmap not . switch $
    mconcat
      [ long "no-type"
      , help "Do not show the inferred type of the input"
      ]

showMetas :: Parser Bool
showMetas =
  switch $
    mconcat
      [ long "metas"
      , help "Show all metavariables"
      ]

version :: Parser (a -> a)
version =
  infoOption ("Pirec version " <> showVersion Paths.version) $
    mconcat
      [ short 'v'
      , long "version"
      , help "Show version"
      , hidden
      ]
