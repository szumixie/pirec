module Pirec (main, mainWith, mainWithInput) where

import Relude hiding (runReader)
import Relude.Extra.Tuple (traverseToSnd)

import Data.Text.IO qualified as Text
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Traversable (for)
import Main.Utf8 (withUtf8)
import System.IO (hPutStr)

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
import Optics
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

import Pirec.Options

main :: IO ()
main = withUtf8 $ mainWith =<< getOpts

mainWith :: Options -> IO ()
mainWith opts =
  uncurry (mainWithInput opts) =<< case opts ^. #input of
    File fp -> traverseToSnd Utf8.readFile fp
    Stdin -> ("<stdin>",) <$> Text.getContents

mainWithInput :: Options -> FilePath -> Text -> IO ()
mainWithInput opts fp input = do
  raw <- case parseRaw fp input of
    Right raw -> pure raw
    Left err -> hPutStr stderr (MP.errorBundlePretty err) *> exitFailure
  runM do
    ((mlookup, (t, a)), metas) <-
      metaCtxToIO $
        errorToIO (runReader Ctx.empty $ infer raw) >>= \case
          Right res -> (,res) <$> metaLookup
          Left err -> do
            err <- prettyElabError err fp input
            embed $ hPutStr stderr err *> exitFailure
    runMetaLookup mlookup do
      t <-
        if not $ opts ^. #showTerm
          then pure Nothing
          else Just . prettyTerm <$> if opts ^. #eval then normal t else pure t
      a <-
        if not $ opts ^. #showType
          then pure Nothing
          else do
            a <- prettyValue Ctx.empty a
            pure . Just . PP.indent 2 $ PP.colon <+> a
      metas <-
        if not $ opts ^. #showMetas
          then pure Nothing
          else do
            prettyMetas <- for metas \(meta, t) -> do
              t <- case t of
                Nothing -> pure "?"
                Just t -> prettyValue Ctx.empty t
              pure $ PP.pretty meta <+> PP.equals <+> t
            pure . Just $
              PP.vsep
                [ "Metavariables:"
                , PP.indent 2 (PP.vsep prettyMetas)
                ]
      let doc = case catMaybes [t, a, metas] of
            [] -> "Nothing to show"
            ps -> PP.vsep ps
      embed . PP.putDoc $ doc <> PP.line
