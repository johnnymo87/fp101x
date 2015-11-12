module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Prelude hiding ((||), (&&))

main :: IO ()
main = hspec $ do
  describe "halve" $ do
    it "splits an even-lengthed list into two halves" $ do
      -- let halve xs = (take n xs, drop n xs) where n = length xs `div` 2
      -- let halve xs = splitAt (length xs `div` 2) xs
      let halve xs = splitAt (div (length xs) 2) xs
      halve [1,2,3,4,5,6] `shouldBe` ([1,2,3],[4,5,6])

  describe "safetail" $ do
    it "behaves like tail, except that it maps an empty list to itself" $ do
      -- let safetail xs = if null xs then [] else tail xs
      -- let { safetail [] = [] ; safetail (_ : xs) = xs }
      -- let safetail xs | null xs = [] | otherwise = tail xs
      let { safetail = \xs -> case xs of [] -> [] ; (_ : xs) -> xs }
      safetail [1,2,3] `shouldBe` [2,3]
      safetail ([] :: [Int]) `shouldBe` []

  describe "logical disjunction operator" $ do
    -- let { False || False = False ; _ || _ = True }
    -- let { False || b = b ; True || _ = True }
    -- let b || c | b == c = b | otherwise = True
    -- let b || c | b == c = c | otherwise = True
    let { b || False = b ; _ || True = True }

    it "True True" $ do
      True || True `shouldBe` True

    it "False True" $ do
      False || True `shouldBe` True

    it "True False" $ do
      True || False `shouldBe` True

    it "False False" $ do
      False || False `shouldBe` False

  describe "logical conjunction operator" $ do
    let a && b = if b then a else False

    it "True True" $ do
      True && True `shouldBe` True

    it "False True" $ do
      False && True `shouldBe` False

    it "True False" $ do
      True && False `shouldBe` False

    it "False False" $ do
      False && False `shouldBe` False

  describe "mult x y z = x * y * z implemented with lambda expressions" $ do
    let mult x y z = x * y * z

    it "is currying" $ do
      (\x -> (\y -> (\z -> x * y * z))) 1 2 3 `shouldBe` mult 1 2 3

  describe "remove :: Int -> [a] -> [a]" $ do
    let remove n xs = take n xs ++ drop (n + 1) xs

    it "takes a number n and a list and removes the element at position n from the list" $ do
      remove 0 [1, 2, 3, 4] `shouldBe` [2, 3, 4]

