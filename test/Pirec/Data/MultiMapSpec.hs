module Pirec.Data.MultiMapSpec (spec, int, assocList, multiMap) where

import Relude hiding (catMaybes, filter, mapMaybe, uncons)
import Relude.Extra.Tuple (dup)

import Data.Functor.Plus
import Data.List qualified as List
import GHC.Exts qualified as Exts
import Witherable

import Optics

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Test.Hspec
import Test.Hspec.Hedgehog

import Pirec.Data.MultiMap (MultiMap)
import Pirec.Data.MultiMap qualified as MM

int :: Gen Int
int = G.int R.linearBounded

assocList :: Int -> Gen a -> Gen [(Int8, a)]
assocList n g = G.list (R.linear 0 n) ((,) <$> G.int8 R.constantBounded <*> g)

multiMap :: Int -> Gen a -> Gen (MultiMap Int8 a)
multiMap n = fmap fromList . assocList n

validToAscList :: MultiMap Int8 a -> Maybe [(Int8, a)]
validToAscList xs
  | MM.valid xs = Just $ sortWith fst (Exts.toList xs)
  | otherwise = Nothing

spec :: Spec
spec = parallel do
  specify "fromList/toList" $ hedgehog do
    xs <- forAll $ assocList 1000 int
    validToAscList (fromList xs) === Just (sortWith fst xs)
  describe "Filterable" do
    specify "catMaybes" $ hedgehog do
      xs <- forAll $ assocList 1000 (G.maybe int)
      validToAscList (catMaybes $ fromList xs)
        === Just (sortWith fst $ xs & mapMaybe \(k, mx) -> (k,) <$> mx)
    specify "identity" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      mapMaybe Just xs === xs
  describe "Apply" do
    specify "associativity" $ hedgehog do
      xs <- forAll $ multiMap 300 int
      ys <- forAll $ multiMap 300 int
      zs <- forAll $ multiMap 300 int
      let assoc ((x, y), z) = (x, (y, z))
      (assoc <$> liftF2 (,) (liftF2 (,) xs ys) zs)
        === liftF2 (,) xs (liftF2 (,) ys zs)
  describe "Semigroup" do
    specify "<>" $ hedgehog do
      xs <- forAll $ assocList 500 int
      ys <- forAll $ assocList 500 int
      validToAscList (fromList xs <> fromList ys)
        === Just (sortWith fst $ xs <> ys)
    specify "associativity" $ hedgehog do
      xs <- forAll $ multiMap 300 int
      ys <- forAll $ multiMap 300 int
      zs <- forAll $ multiMap 300 int
      (xs <> ys) <> zs === xs <> (ys <> zs)
  describe "Monoid" do
    specify "mempty" do
      validToAscList @Int mempty `shouldBe` Just []
    specify "left identity" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      mempty <> xs === xs
    specify "right identity" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      xs <> mempty === xs
  describe "One" do
    specify "one" $ hedgehog do
      k <- forAll $ G.int8 R.constantBounded
      x <- forAll int
      validToAscList (one (k, x)) === Just [(k, x)]
  describe "Ixed" do
    describe "ix" do
      specify "preview" $ hedgehog do
        xs <- forAll $ assocList 1000 int
        k <- forAll $ G.int8 R.constantBounded
        i <- forAll $ G.int (R.linear 0 10)
        preview (ix (k, i)) (fromList @(MultiMap Int8 Int) xs)
          === (xs & filter ((== k) . fst) & preview (ix i) <&> snd)
      specify "set" $ hedgehog do
        x <- forAll int
        xs <- forAll $ assocList 1000 int
        k <- forAll $ G.int8 R.constantBounded
        i <- forAll $ G.int (R.linear 0 10)
        let go j = \case
              [] -> []
              t@(k', _) : ys
                | k' /= k -> t : go j ys
                | j >= i -> (k', x) : ys
                | otherwise -> t : go (j + 1) ys
        validToAscList (set (ix (k, i)) x (fromList xs))
          === Just (sortWith fst $ go 0 xs)
  describe "intersectionWith" do
    specify "validity" $ hedgehog do
      xs <- forAll $ multiMap 500 int
      ys <- forAll $ multiMap 500 int
      assert $ MM.valid (MM.intersectionWith (,) xs ys)
    specify "self intersection" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      MM.intersectionWith (,) xs xs === (dup <$> xs)
  describe "matchWith" do
    specify "validity" $ hedgehog do
      xs <- forAll $ multiMap 500 int
      ys <- forAll $ multiMap 500 int
      assert $ maybe True MM.valid (MM.matchWith (,) xs ys)
    specify "self match" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      MM.matchWith (,) xs xs === Just (dup <$> xs)
  describe "difference" do
    specify "specification" $ hedgehog do
      xs <- forAll $ assocList 500 int
      ys <- forAll $ assocList 500 int
      validToAscList (MM.difference (fromList xs) (fromList ys))
        === Just (sortWith fst $ List.deleteFirstsBy ((==) `on` fst) xs ys)
    specify "left division" $ hedgehog do
      xs <- forAll $ multiMap 500 int
      ys <- forAll $ multiMap 500 int
      MM.difference (xs <> ys) xs === ys
    specify "self difference" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      MM.difference xs xs === mempty
  describe "isKeySubsetOf" do
    specify "specification" $ hedgehog do
      xs <- forAll $ assocList 500 int
      ys <- forAll $ assocList 500 int
      fromList xs `MM.isSubsetOf` fromList ys
        === sort (fst <$> xs) `List.isSubsequenceOf` sort (fst <$> ys)
    specify "self subset" $ hedgehog do
      xs <- forAll $ multiMap 1000 int
      assert $ xs `MM.isSubsetOf` xs
