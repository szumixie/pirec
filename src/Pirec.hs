module Pirec (main) where

import Relude hiding (runReader)

import Data.Text.IO.Utf8 qualified as Utf8
import Main.Utf8 (withUtf8)
import System.IO (hPutStr)

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
import Options.Applicative
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Text.Megaparsec qualified as MP

import Pirec.Effect.Meta
import Pirec.Elab (infer)
import Pirec.Elab.Context qualified as Ctx
import Pirec.Elab.Error (prettyElabError)
import Pirec.Eval (normal)
import Pirec.Pretty (prettyTerm, prettyValue)
import Pirec.Syntax.Raw.Parse (parseRaw)

newtype Options = Options FilePath
  deriving stock (Show)

opts :: Parser Options
opts = Options <$> strArgument (metavar "PATH")

main :: IO ()
main = withUtf8 do
  Options fp <- execParser $ info (opts <**> helper) mempty
  input <- Utf8.readFile fp
  raw <- case parseRaw fp input of
    Right raw -> pure raw
    Left err -> hPutStr stderr (MP.errorBundlePretty err) *> exitFailure
  runM $ metaCtxToIO do
    (t, a) <-
      errorToIO (runReader Ctx.empty $ infer raw) >>= \case
        Right ta -> pure ta
        Left err -> do
          perr <- prettyElabError err fp input
          embed $ hPutStr stderr perr *> exitFailure
    t <- normal t
    a <- prettyValue Ctx.empty a
    embed . PP.putDoc $ prettyTerm t <> PP.line <> PP.indent 2 (PP.colon <+> a)
