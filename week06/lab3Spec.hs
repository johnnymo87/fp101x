module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lab3

main :: IO ()
main = hspec $ do
  describe "evens" $ do
    it "returns even numbers from a list" $ do
      evens [2,5,6,13,32] `shouldBe` [2,6,32]

  describe "squares" $ do
    it "returns squared numbers from a range of numbers from one to a number" $ do
      squares 4 `shouldBe` [1*1, 2*2, 3*3, 4*4]
      squares 0 `shouldBe` []

  describe "squares'" $ do
    it "returns m squared numbers from a range starting with n+1" $ do
      squares' 4 2 `shouldBe` [3*3, 4*4, 5*5, 6*6]
      squares' 2 0 `shouldBe` [1*1, 2*2]
      squares' 0 2 `shouldBe` []
      squares' 0 0 `shouldBe` []

  describe "coords" $ do
    it "returns returns a list of all coordinate pairs on an [0..m] × [0..n]" $ do
      coords 1 1 `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
      coords 1 2 `shouldBe` [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]

