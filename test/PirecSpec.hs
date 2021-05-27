module PirecSpec (spec) where

import Relude

import System.Directory (listDirectory)
import System.IO.Silently (silence)

import Test.Hspec

import Pirec
import Pirec.Options

spec :: Spec
spec =
  describe "examples run without error" do
    fps <- runIO (listDirectory "examples")
    parallel $ for_ fps \fp -> do
      specify fp do
        silence $ mainWith (defaultOpts $ File ("examples/" <> fp))
