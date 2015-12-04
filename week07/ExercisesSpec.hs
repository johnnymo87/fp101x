module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Exercises

main :: IO ()
main = hspec $ do
  describe "[f x | x <- xs, p x] expressed using higher-order functions" $ do
    it "works" $ property $
      let p = even
          f = (+1)
      in \xs -> map f (filter p xs) == [f x | x <- (xs :: [Int]), p x]

  describe "prelude function all" $ do
    it "works" $ property $
      -- let all' p xs = and (map p xs)
      -- let all' p = and . map p
      -- let all' p = not . any (not . p)
      -- let all' p xs = foldl (&&) True (map p xs)
      -- let all' p xs = foldr (&&) False (map p xs)
      let all' p = foldr (&&) True . map p
      in \xs -> all' even xs == all even (xs :: [Int])

  describe "prelude function any" $ do
    it "works" $ property $
      -- let any' p = or . map p
      -- let any' p xs = length (filter p xs) > 0
      -- let any' p = not . null . dropWhile (not . p)
      -- let any' p xs = not (all (\x -> not (p x)) xs)
      let any' p xs = foldr (\x acc -> (p x) || acc) False xs
      in \xs -> any' even xs == any even (xs :: [Int])

  describe "prelude function takeWhile" $ do
    it "works" $ property $
      let { takeWhile' _ [] = [] ; takeWhile' p (x : xs) | p x = x : takeWhile' p xs | otherwise = [] }
      in \xs -> takeWhile' even xs == takeWhile even (xs :: [Int])

  describe "prelude function dropWhile" $ do
    it "works" $ property $
      let { dropWhile' _ [] = [] ; dropWhile' p (x : xs) | p x = dropWhile' p xs | otherwise = x : xs }
      in \xs -> dropWhile' even xs == dropWhile even (xs :: [Int])

  describe "prelude function map" $ do
    it "works" $ property $
      let map' f = foldl (\acc x -> acc ++ [f x]) []
      in \xs -> map' even xs == map even (xs :: [Int])

  describe "prelude function filter" $ do
    it "works" $ property $
      let filter' p = foldr (\x acc -> if p x then x : acc else acc) []
      in \xs -> filter' even xs == filter even (xs :: [Int])

  describe "a function that converts a list of Integers into the Integer that the list represents" $ do
    let dec2int xs = sum [x * 10 ** p| (x, p) <- zip (reverse xs) [0..]]
    let dec2int = foldl (\acc x -> acc * 10 + x) 0

    it "list contains positive natural numbers" $ do
      dec2int [2,3,4,5] `shouldBe` 2345

    it "list contains nothing" $ do
      dec2int [] `shouldBe` 0

    it "list contains zeroes" $ do
      dec2int [0,0,0,0] `shouldBe` 0

  describe "curry & uncurry" $ do
    let add (a, b) = a + b

    it "add" $ do
      add (1, 2) `shouldBe` 3

    it "curry" $ do
      let curry f = \x y -> f (x, y)
      curry add 1 2 `shouldBe` 3

    it "uncurry" $ do
      let uncurry f = \(x, y) -> f x y
      uncurry (curry add) (1, 2) `shouldBe` 3

  describe "int2bin" $ do
    it "converts a non-negative integer into a binary number, with the least significant bit first" $ do
      int2bin 13 `shouldBe` [1,0,1,1]
      int2bin (-0) `shouldBe` []

  describe "chop8" $ do
    it "takes a list of bits and chops it into lists of at most eight bits" $ do
      chop8 [1..24] `shouldBe` [[1..8],[9..16],[17..24]]

  describe "unfold" $ do
    it "works with int2bin" $ do
      let int2bin' = unfold (== 0) (`mod` 2) (`div` 2)
      int2bin' 13 `shouldBe` int2bin 13
      int2bin' (-0) `shouldBe` int2bin (-0)

    it "works with chop8" $ do
      let chop8' = unfold null (take 8) (drop 8)
      chop8' [1..24] `shouldBe` chop8 [1..24]

    it "works with map" $ do
      let map' f = unfold null (f . head) tail
      map' even [1..10] `shouldBe` map even [1..10]

    it "works with iterate" $ do
      let iterate' f = unfold (const False) id f
      take 5 (iterate' (+1) 1) `shouldBe` take 5 (iterate (+1) 1)
