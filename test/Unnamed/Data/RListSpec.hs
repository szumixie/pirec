module Unnamed.Data.RListSpec (spec) where

import Relude hiding (uncons)

import Optics

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Test.Hspec
import Test.Hspec.Hedgehog

import Unnamed.Data.RList (RList)
import Unnamed.Data.RList qualified as R

int :: Gen Int
int = G.int R.linearBounded

list :: Gen a -> Gen [a]
list = G.list $ R.linear 0 1000

shortList :: Gen a -> Gen [a]
shortList = G.list $ R.linear 0 30

spec :: Spec
spec = parallel do
  specify "toList/fromList" $ hedgehog do
    xs <- forAll $ list int
    toList @RList (fromList xs) === xs
  describe "Functor" do
    specify "fmap" $ hedgehog do
      xs <- forAll $ list int
      toList @RList ((+ 1) <$> fromList xs) === ((+ 1) <$> xs)
  describe "Foldable" do
    specify "fold" $ hedgehog do
      xss <- forAll $ shortList (shortList int)
      fold @RList (fromList xss) === fold xss
    specify "null" $ hedgehog do
      xs <- forAll $ shortList int
      null @RList (fromList xs) === null xs
    specify "length" $ hedgehog do
      xs <- forAll $ list int
      length @RList (fromList xs) === length xs
  describe "Traversable" do
    specify "sequenceA" $ hedgehog do
      xss <-
        forAll $ shortList ((,) <$> shortList int <*> int)
      (toList @RList <$> sequenceA (fromList xss)) === sequenceA xss
  describe "FunctorWithIndex" do
    specify "imap" $ hedgehog do
      xs <- forAll $ list int
      toList @RList (imap (,) (fromList xs)) === imap (,) xs
  describe "FoldableWithIndex" do
    specify "ifoldMap" $ hedgehog do
      xs <- forAll $ shortList (shortList int)
      ifoldMap @_ @RList (:) (fromList xs) === ifoldMap (:) xs
  describe "TraversableWithIndex" do
    specify "itraverse" $ hedgehog do
      let f i (xs, n) = (i : xs, (i, n))
      xs <- forAll $ shortList ((,) <$> shortList int <*> int)
      (toList @RList <$> itraverse f (fromList xs)) === itraverse f xs
  specify "empty" $ hedgehog do
    toList (R.empty @Int) === []
  specify "cons" $ hedgehog do
    x <- forAll int
    xs <- forAll $ list int
    toList (R.cons x (fromList xs)) === x : xs
  specify "uncons" $ hedgehog do
    xs <- forAll $ list int
    (fmap toList <$> R.uncons (fromList xs)) === uncons xs
  describe "Ixed" do
    specify "matching" $ hedgehog do
      xs <- forAll $ list int
      i <- forAll $ G.int (R.constant 0 (length xs - 1))
      matching (ix i) (fromList xs) === matching (ix i) xs
    specify "set" $ hedgehog do
      x <- forAll int
      xs <- forAll $ list int
      i <- forAll $ G.int (R.constant 0 (length xs - 1))
      toList @RList (set (ix i) x (fromList xs)) === set (ix i) x xs
