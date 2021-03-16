module Unnamed (main) where

import Relude

import Data.Text.IO.Utf8 qualified as Utf8
import Main.Utf8 (withUtf8)
import System.IO (hPutStr)

import Control.Effect
import Control.Effect.Error
import Options.Applicative
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Text.Megaparsec qualified as MP

import Unnamed.Effect.Meta
import Unnamed.Elab (infer)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Elab.Error (prettyElabError)
import Unnamed.Eval (normal)
import Unnamed.Syntax.Core.Pretty (prettyTerm)
import Unnamed.Syntax.Raw.Parse qualified as R
import Unnamed.Value.Pretty (prettyValue)

newtype Options = Options FilePath
  deriving stock (Show)

opts :: Parser Options
opts = Options <$> strArgument (metavar "PATH")

main :: IO ()
main = withUtf8 do
  Options fp <- execParser $ info (opts <**> helper) mempty
  content <- Utf8.readFile fp
  raw <- case MP.parse R.parser fp content of
    Right raw -> pure raw
    Left err -> hPutStr stderr (MP.errorBundlePretty err) *> exitFailure
  runM $ metaCtxToIO do
    (t, va) <-
      errorToIO (infer Ctx.empty raw) >>= \case
        Right tva -> pure tva
        Left err -> do
          perr <- prettyElabError err
          embed $ PP.hPutDoc stderr perr *> exitFailure
    t' <- normal t
    pa <- prettyValue Ctx.empty va
    embed . PP.putDoc $ prettyTerm t' <+> PP.colon <+> pa
