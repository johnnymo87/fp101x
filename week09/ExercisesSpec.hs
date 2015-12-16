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

  describe "compare" $ do
    describe "decides whether the given Integer occurs in the given Tree" $ do
      it "matches first position, sorted asc" $ do
        occurs 1 (Node (Leaf 1) 3 (Leaf 5)) `shouldBe` True

      -- it "matches first position, sorted desc" $ do
      --   occurs 5 (Node (Leaf 5) 3 (Leaf 1)) `shouldBe` True

      it "matches middle position, sorted asc" $ do
        occurs 3 (Node (Leaf 1) 3 (Leaf 5)) `shouldBe` True

      -- it "matches middle position, sorted desc" $ do
      --   occurs 3 (Node (Leaf 5) 3 (Leaf 1)) `shouldBe` True

      it "matches last position, sorted asc" $ do
        occurs 5 (Node (Leaf 1) 3 (Leaf 5)) `shouldBe` True

      -- it "matches last position, sorted desc" $ do
      --   occurs 1 (Node (Leaf 5) 3 (Leaf 1)) `shouldBe` True

      it "does not match unmatched" $ do
        occurs 2 (Node (Leaf 1) 3 (Leaf 5)) `shouldBe` False

  describe "leaves" $ do
    describe "counts the number of leaves" $ do
      it "works 0 levels deep" $ do
        leaves (Leaf' 1) `shouldBe` 1

      it "works 1 level deep" $ do
        leaves (Node' (Leaf' 1) (Leaf' 5)) `shouldBe` 2

      it "works 2 level deep" $ do
        leaves (Node' (Node' (Leaf' 1) (Leaf' 5)) (Node' (Leaf' 1) (Leaf' 5))) `shouldBe` 4

  describe "balanced" $ do
    describe "returns true if the number of leaves in the left and right subtree of every node differs by at most one, with leaves themselves being trivially balanced" $ do
      it "True 0 levels deep" $ do
        balanced (Leaf' 1) `shouldBe` True

      it "True 1 level deep, balanced" $ do
        balanced (Node' (Leaf' 1) (Leaf' 5)) `shouldBe` True

      it "True 1 level deep, left +1" $ do
        balanced (Node' (Node' (Leaf' 1) (Leaf' 5)) (Leaf' 5)) `shouldBe` True

      -- (
      --   Node' (
      --     Node' (
      --       Node' (Leaf' 1) (Leaf' 5)
      --     ) (Leaf' 5)
      --   ) (Leaf' 5)
      -- )
      it "False 1 level deep, left +2" $ do
        balanced (Node' (Node' (Node' (Leaf' 1) (Leaf' 5)) (Leaf' 5)) (Leaf' 5)) `shouldBe` False

  describe "halve" $ do
    describe "splits the list in half, favoring the right side" $ do
      it "empty list" $ do
        halve ([] :: [Integer]) `shouldBe` ([],[])

      it "singleton list" $ do
        halve [1] `shouldBe` ([],[1])

      it "list of 2 elements" $ do
        halve [1,2] `shouldBe` ([1],[2])

      it "list of 3 elements" $ do
        halve [1,2,3] `shouldBe` ([1],[2,3])

  describe "balance" $ do
    describe "converts a finite, non-empty, non-partial, non-bottom list of non-bottom integers into a balanced tree" $ do
      it "singleton list" $ do
        balance [1] `shouldBe` Leaf' 1

      it "list of 2 elements" $ do
        balance [1,2] `shouldBe` Node' (Leaf' 1) (Leaf' 2)

      it "list of 3 elements" $ do
        balance [1,2,3] `shouldBe` Node' (Leaf' 1) (Node' (Leaf' 2) (Leaf' 3))
