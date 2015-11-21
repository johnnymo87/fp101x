module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Exercises

main :: IO ()
main = hspec $ do
  describe "sum100" $ do
    it "sums the squares of 1 through 100" $ do
      let sum100 = sum [x^2 | x <- [1..100]]
      sum100 `shouldBe` 338350

  describe "replicate" $ do
    it "produces a list of identical elements" $ do
      let replicate' n a = [a | _ <- [1..n]]
      replicate' 3 True `shouldBe` replicate 3 True

  describe "pyths" $ do
    it "returns all positive triples that are pythagorean (x^2 + y^2 = z^2)" $ do
      let pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
      pyths 10 `shouldBe` [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

  describe "factors" $ do
    it "returns all the factors of a number" $ do
      factors 12 `shouldBe` [1,2,3,4,6]

  describe "perfects" $ do
    it "equals the sum of its factors, excluding the number itself" $ do
      perfects 500 `shouldBe` [6,28,496]

  describe "refactoring one list comprehesion into two" $ do
    it "can be done" $ do
      let run' = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]
      run' `shouldBe` [(x, y) | x <- [1,2,3], y <- [4,5,6]]

  describe "find" $ do
    it "returns a list of values matched from keys from key-value pairs" $ do
      find 'a' [('a',1),('b',2),('a',1),('c',3)] `shouldBe` [1,1]

  describe "positions" $ do
    it "returns a list of indicies from an element's occurences in a list" $ do
      positions 'a' "abaca" `shouldBe` [0,2,4]

  describe "scalarproduct" $ do
    it "multiplies pairwise the elements of two lists and then sums them" $ do
      scalarproduct [1, 2, 3] [4, 5, 6] `shouldBe` 32

  describe "riffle" $ do
    it "takes two lists of the same length and interleaves their elements in turn about order" $ do
      let riffle xs ys = concat [[x, y] | (x, y) <- zip xs ys]
      riffle [1, 2, 3] [4, 5, 6] `shouldBe` [1,4,2,5,3,6]

  describe "divides" $ do
    describe "decides if one integer is divisible by another" $ do
      it "is true when it divides cleanly" $ do
        divides 15 3 `shouldBe` True

      it "is false when it does not divide cleanly" $ do
        divides 15 6 `shouldBe` False

  describe "divisors" $ do
    it "returns the divisors of a natural number" $ do
      let divisors x = [n | n <- [1..x], x `divides` n]
      divisors 15 `shouldBe` [1,3,5,15]

