module Pirec.Data.MultiMapAlterSpec (spec) where

import Relude hiding (uncons)
import Relude.Extra.Tuple (dup)

import Optics

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Test.Hspec
import Test.Hspec.Hedgehog

import Pirec.Data.MultiMap qualified as MM
import Pirec.Data.MultiMapAlter (MultiMapAlter)
import Pirec.Data.MultiMapAlter qualified as MMA

import Pirec.Data.MultiMapSpec hiding (spec)

multiMapAlter :: Int -> Gen a -> Gen (MultiMapAlter Int8 a)
multiMapAlter n g =
  assocList n (G.maybe g) <&> foldMap' \(k, x) ->
    maybe (MMA.singleDelete k) (MMA.singleInsert k) x

keys :: Int -> Gen [Int8]
keys n = G.list (R.linear 0 n) (G.int8 R.constantBounded)

allInsert :: [(Int8, a)] -> MultiMapAlter Int8 a
allInsert = foldMap' (uncurry MMA.singleInsert)

allDelete :: [Int8] -> MultiMapAlter Int8 b
allDelete = foldMap' MMA.singleDelete

spec :: Spec
spec = parallel do
  specify "valid" $ hedgehog do
    xs <- forAll $ multiMapAlter 1000 int
    assert $ MMA.valid xs
  describe "Semigroup" do
    specify "<>" $ hedgehog do
      xs <- forAll $ multiMapAlter 500 int
      ys <- forAll $ multiMapAlter 500 int
      assert $ MMA.valid (xs <> ys)
    specify "associativity" $ hedgehog do
      xs <- forAll $ multiMapAlter 300 int
      ys <- forAll $ multiMapAlter 300 int
      zs <- forAll $ multiMapAlter 300 int
      (xs <> ys) <> zs === xs <> (ys <> zs)
    specify "inverse" $ hedgehog do
      xs <- forAll $ assocList 1000 int
      allDelete (fst <$> xs) <> allInsert xs === mempty
  describe "Monoid" do
    specify "mempty" do
      mempty `shouldSatisfy` MMA.valid @Int8 @Int
    specify "left identity" $ hedgehog do
      xs <- forAll $ multiMapAlter 1000 int
      mempty <> xs === xs
    specify "right identity" $ hedgehog do
      xs <- forAll $ multiMapAlter 1000 int
      xs <> mempty === xs
  describe "apply" do
    specify "validity" $ hedgehog do
      xs <- forAll $ multiMapAlter 500 int
      ys <- forAll $ multiMap 500 int
      assert $ MM.valid (MMA.apply xs ys)
    specify "all insert" $ hedgehog do
      xs <- forAll $ assocList 500 int
      ys <- forAll $ multiMap 500 int
      MMA.apply (allInsert xs) ys === fromList xs <> ys
    specify "all delete" $ hedgehog do
      xs <- forAll $ assocList 500 int
      ys <- forAll $ multiMap 500 int
      MMA.apply (allDelete $ fst <$> xs) ys
        === MM.difference ys (fromList xs)
  describe "lookup" do
    specify "all insert" $ hedgehog do
      xs <- forAll $ assocList 1000 int
      k <- forAll $ G.int8 R.constantBounded
      i <- forAll $ G.int (R.linear 0 10)
      let ys = snd <$> filter ((== k) . fst) xs
      MMA.lookup k i (allInsert xs) === maybeToRight (i - length ys) (ys !!? i)
    specify "all delete" $ hedgehog do
      ks <- forAll $ keys 1000
      k <- forAll $ G.int8 R.constantBounded
      i <- forAll $ G.int (R.linear 0 10)
      MMA.lookup k i (allDelete @Int ks)
        === Left (i + length (filter (== k) ks))
  describe "match" do
    specify "validity" $ hedgehog do
      xs <- forAll $ multiMapAlter 500 int
      ys <- forAll $ multiMapAlter 500 int
      assert $ maybe True MMA.valid (MMA.match (,) xs ys)
    specify "self match" $ hedgehog do
      xs <- forAll $ multiMapAlter 1000 int
      MMA.match (,) xs xs === Just (dup <$> xs)
  describe "ifoldedAlter" do
    specify "all insert" $ hedgehog do
      xs <- forAll $ assocList 1000 int
      (xs & allInsert & itoListOf MMA.ifoldedAlter & sortWith fst)
        === (fmap Just <$> sortWith fst xs)
    specify "all delete" $ hedgehog do
      ks <- forAll $ keys 1000
      (ks & allDelete & itoListOf MMA.ifoldedAlter & sortWith fst)
        === ((,Nothing @Int) <$> sort ks)
