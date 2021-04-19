module Unnamed.Data.RListSpec (spec) where

import Relude hiding (uncons)

import Optics

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Test.Hspec
import Test.Hspec.Hedgehog

import Unnamed.Data.RList (RList)
import Unnamed.Data.RList qualified as RList

int :: Gen Int
int = G.int R.linearBounded

list :: Int -> Gen a -> Gen [a]
list n = G.list $ R.linear 0 n

validToList :: RList a -> Maybe [a]
validToList xs
  | RList.valid xs = Just $ toList xs
  | otherwise = Nothing

spec :: Spec
spec = parallel do
  specify "toList/fromList" $ hedgehog do
    xs <- forAll $ list 1000 int
    tripping xs fromList validToList
  describe "Functor" do
    specify "fmap" $ hedgehog do
      xs <- forAll $ list 1000 int
      validToList ((+ 1) <$> fromList xs) === Just ((+ 1) <$> xs)
  describe "Foldable" do
    specify "fold" $ hedgehog do
      xss <- forAll $ list 100 (list 10 int)
      fold @RList (fromList xss) === fold xss
    specify "null" $ hedgehog do
      xs <- forAll $ list 100 int
      null @RList (fromList xs) === null xs
    specify "length" $ hedgehog do
      xs <- forAll $ list 1000 int
      length @RList (fromList xs) === length xs
  describe "Traversable" do
    specify "sequenceA" $ hedgehog do
      xss <- forAll $ list 100 ((,) <$> list 10 int <*> int)
      traverse validToList (sequenceA (fromList xss)) === Just (sequenceA xss)
  specify "empty" do
    validToList (RList.empty @Int) `shouldBe` Just []
  specify "cons" $ hedgehog do
    x <- forAll int
    xs <- forAll $ list 1000 int
    validToList (RList.cons x (fromList xs)) === Just (x : xs)
  specify "uncons" $ hedgehog do
    xs <- forAll $ list 1000 int
    traverse (traverse validToList) (RList.uncons (fromList xs))
      === Just (uncons xs)
  describe "Ixed" do
    specify "matching" $ hedgehog do
      xs <- forAll $ list 1000 int
      i <- forAll $ G.int (R.constant 0 (length xs - 1))
      matching (ix i) (fromList xs) === matching (ix i) xs
    specify "set" $ hedgehog do
      x <- forAll int
      xs <- forAll $ list 1000 int
      i <- forAll $ G.int (R.constant 0 (length xs - 1))
      validToList (set (ix i) x (fromList xs)) === Just (set (ix i) x xs)
  describe "FunctorWithIndex" do
    specify "imap" $ hedgehog do
      xs <- forAll $ list 1000 int
      validToList (imap (,) (fromList xs)) === Just (imap (,) xs)
  describe "FoldableWithIndex" do
    specify "ifoldMap" $ hedgehog do
      xs <- forAll $ list 100 (list 10 int)
      ifoldMap @_ @RList (:) (fromList xs) === ifoldMap (:) xs
  describe "TraversableWithIndex" do
    specify "itraverse" $ hedgehog do
      let f i (xs, n) = (i : xs, (i, n))
      xs <- forAll $ list 100 ((,) <$> list 10 int <*> int)
      traverse validToList (itraverse f (fromList xs)) === Just (itraverse f xs)
