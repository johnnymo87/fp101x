module Lab4Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lab4

main :: IO ()
main = hspec $ do
  describe "triangle" $ do
    it "works like sum [0..n]" $ property $
      \x ->
        triangle x == sum [0..(x :: Integer)]

  describe "count" $ do
    it "counts how many times a given value occurs in a list" $ do
      count 'e' "The quick brown fox jumped over the lazy dog." `shouldBe` 4

  describe "euclid" $ do
    it "returns the greatest common factor of two integers" $ do
      euclid (13404, 8832) `shouldBe` 12

  describe "funkyMap" $ do
    it "applies f to all elements at even positions and g to all elements at odd positions" $ do
      funkyMap (+10) (+100) [1, 2, 3, 4, 5] `shouldBe` [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5]
