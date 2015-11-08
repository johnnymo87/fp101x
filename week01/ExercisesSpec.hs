module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Exercises

main :: IO ()
main = hspec $ do
  describe "init" $ do
    it "returns all but the last element of the list" $ do
      (\xs -> take (length xs -1) xs) [1,2,3,4,5] `shouldBe` init [1,2,3,4,5]
      (\xs -> reverse $ tail $ reverse xs) [1,2,3,4,5] `shouldBe` init [1,2,3,4,5]

  describe "product" $ do
    it "returns the product of a list of numbers" $ do
      let { product' [] = 1 ; product' (x:xs) = x * product' xs }
      product' [2,3,4] `shouldBe` product [2,3,4]

  describe "qsort'" $ do
    it "sorts the list reverse numerically" $ do
      qsort' [2,13,3,1,5,8,1] `shouldBe` [13,8,5,3,2,1,1]

