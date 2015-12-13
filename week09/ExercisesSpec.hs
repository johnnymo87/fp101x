module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Exercises

main :: IO ()
main = hspec $ do
  describe "natToInteger" $ do
    it "converts any non-bottom, non-partial, finite natural number (note: 0 is a natural number according to this definition), into the corresponding Integer value" $ do
      natToInteger Zero `shouldBe` 0
      natToInteger (Succ (Succ Zero)) `shouldBe` 2
