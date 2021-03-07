module Unnamed (main) where

import Relude

import Data.Text.IO.Utf8 qualified as Utf8
import Main.Utf8 (withUtf8)
import System.IO (hPutStr)

import Options.Applicative
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Text.Megaparsec qualified as MP

import Unnamed.Elab (infer)
import Unnamed.Elab.Context qualified as Ctx
import Unnamed.Elab.Error (prettyElabError)
import Unnamed.Eval (normal)
import Unnamed.Syntax.Core.Pretty (prettyTerm)
import Unnamed.Syntax.Raw.Parse qualified as R
import Unnamed.Value.Pretty (prettyValue)

newtype Options = Options {optionsFilePath :: FilePath}
  deriving stock (Show)

opts :: Parser Options
opts = Options <$> strArgument (metavar "PATH")

main :: IO ()
main = withUtf8 do
  Options{optionsFilePath = fp} <- execParser $ info (opts <**> helper) mempty
  content <- Utf8.readFile fp
  raw <- case MP.parse R.parser fp content of
    Right raw -> pure raw
    Left err -> hPutStr stderr (MP.errorBundlePretty err) *> exitFailure
  (t, a) <- case infer Ctx.empty raw of
    Right tup -> pure tup
    Left err -> PP.hPutDoc stderr (prettyElabError err) *> exitFailure
  PP.putDoc $ prettyTerm (normal t) <+> PP.colon <+> prettyValue Ctx.empty a
