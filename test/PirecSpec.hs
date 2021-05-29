module PirecSpec (spec) where

import Relude

import System.Directory
import System.FilePath
import System.IO.Silently (silence)

import Optics

import Test.Hspec

import Pirec
import Pirec.Options

spec :: Spec
spec = do
  describe "examples run without error" do
    fps <- runIO (listDirectory "examples")
    for_ fps \fp -> do
      when ("pirec" `isExtensionOf` fp) do
        specify fp do
          silence $ mainWith (defaultOpts (File $ "examples" </> fp))
  describe "looping examples elaborate without error" do
    fps <- runIO (listDirectory "examples/loop")
    for_ fps \fp -> do
      when ("pirec" `isExtensionOf` fp) do
        specify fp do
          silence $
            mainWith
              (defaultOpts (File $ "examples/loop" </> fp) & #eval .~ False)
