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
      natToInteger (Succ Zero) `shouldBe` 1
      natToInteger (Succ (Succ Zero)) `shouldBe` 2

  describe "integerToNat" $ do
    it "converts any non-bottom, non-partial, finite Integer value >= 0, into the corresponding Nat value" $ do
      integerToNat 0 `shouldBe` Zero
      integerToNat 1 `shouldBe` Succ Zero
      integerToNat 2 `shouldBe` Succ (Succ Zero)

  describe "add" $ do
    it "adds two non-bottom, non-partial, finite natural numbers m and n, such that natToInteger (add m n) = natToInteger m + natToInteger n" $ do
      add Zero Zero `shouldBe` Zero
      add Zero (Succ Zero) `shouldBe` Succ Zero
      add (Succ Zero) (Succ Zero) `shouldBe` Succ (Succ Zero)

  describe "mult" $ do
    it "multiplies two non-bottom, non-partial, finite natural numbers m and n, such that natToInteger (mult m n) = natToInteger m * natToInteger n" $ do
      mult Zero Zero `shouldBe` Zero
      mult Zero (Succ Zero) `shouldBe` Zero
      mult (Succ Zero) (Succ Zero) `shouldBe` (Succ Zero)
      mult (Succ Zero) (Succ (Succ Zero)) `shouldBe` Succ (Succ Zero)
      mult (Succ (Succ Zero)) (Succ (Succ Zero)) `shouldBe` Succ (Succ (Succ (Succ Zero)))
