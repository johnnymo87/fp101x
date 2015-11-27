module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative
import Prelude hiding ((^))
import Exercises

main :: IO ()
main = hspec $ do
  describe "exponentiation operator ^ for non-negative integers (including 0)" $ do
    it "works" $ do
      let { m ^ 0 = 1 ; m ^ n = m * m ^ (n - 1) }
      let { m ^ 0 = 1 ; m ^ n = m * (^) m (n - 1) }
      (^) <$> [0..3] <*> [0..3] `shouldBe` [1,0,0,0,1,1,1,1,1,2,4,8,1,3,9,27]

  describe "a function that decides if all logical values in a list are True" $ do
    let { and' [] = True ; and' (b : bs) = b && and' bs }
    let { and' [] = True ; and' (b : bs) | b = and' bs | otherwise = False }
    let { and' [] = True ; and' (b : bs) | b == False = False | otherwise = and bs }
    let { and' [] = True ; and' (b : bs) = and' bs && b }

    it "works like and" $ property $
      \xs -> and' xs == and (xs :: [Bool])

  describe "a function that concatenates a list of lists" $ do
    let { concat' [] = [] ; concat' (xs : xss) = xs ++ concat xss }

    it "works like concat" $ property $
      \xs -> concat' xs == concat (xs :: [[Bool]])

  describe "a function that produces a list with n identical elements" $ do
    let { replicate' 0 _ = [] ; replicate' n x = x : replicate' (n - 1) x }

    it "works like replicate" $ property $
      forAll (choose (0, 9)) $ \x ->
      forAll (choose (0, 9)) $ \y ->
        replicate' x y == replicate (x :: Int) (y :: Int)

  describe "a function that selects the nth element of a list" $ do
    let { (x : _) !!! 0 = x ; (_ : xs) !!! n = xs !!! (n - 1) }

    it "works like (!!)" $ property $
      \xs ->
      forAll (choose (0, length xs-1)) $ \x ->
        not (null xs) ==> xs !!! x == (xs :: [Int]) !! x

  describe "a function that decides if a value is an element of a list" $ do
    let { elem' _ [] = False ; elem' a (x : xs) | a == x = True | otherwise = elem' a xs }

    it "works like elem" $ property $
      \a xs ->
        elem a xs == elem (a :: Int) (xs :: [Int])

  describe "a function that merges two sorted lists in ascending order to give a single sorted list in ascending order" $ do
    it "works" $
      merge [2,5,6] [1,3,4] `shouldBe` [1,2,3,4,5,6]

  describe "a function that splits a list into two halves whose lengths differ by at most one" $ do
    it "works" $
      halve [1,2,3] `shouldBe` ([1],[2,3])

  describe "a function that sorts by merging together the two lists that result from sorting the two halves of the list separately" $ do
    it "works" $
      msort [7,3,5,4,9,1,3,2,4] `shouldBe` [1,2,3,3,4,4,5,7,9]
