module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Exercises

main :: IO ()
main = hspec $ do
  describe "fibs" $ do
    it "generates the infinite sequence of Fibonacci numbers (i.e. [0, 1, 1, 2, ...])" $ do
      take 7 fibs `shouldBe` [0,1,1,2,3,5,8]

  describe "fib" $ do
    it "returns the n-th Fibonnaci number (counting from zero)" $ do
      fib 7 `shouldBe` 13

  describe "largeFib" $ do
    it "calculates the first Fibonacci number greater than 1000" $ do
      largeFib `shouldBe` 1597

  describe "repeatTree" $ do
    it "is analogous to that of the library function repeat" $ do
      largeFib `shouldBe` 1597

