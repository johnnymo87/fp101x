module Lab2Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lab2

main :: IO ()
main = hspec $ do
  let number = 4012888888881881

  describe "toDigits" $ do
    it "works" $ do
      toDigits number `shouldBe` [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1]

    it "eval" $ do
      let eval xs = foldl (\x y -> y + (10 * x)) 0 xs
      eval (toDigits number) `shouldBe` number

    it "all" $ do
      all (\d -> d >= 0 && d < 10) (toDigits number) `shouldBe` True

    it "length" $ do
      length (toDigits number) `shouldBe` length (show number)

  describe "toDigitsRev" $ do
    it "works" $ do
      reverse (toDigitsRev number) `shouldBe` [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1]

    it "eval" $ do
      let eval xs = foldr (\x y -> x + (10 * y)) 0 xs
      eval (toDigitsRev number) `shouldBe` number

    it "all" $ do
      all (\d -> d >= 0 && d < 10) (toDigitsRev number) `shouldBe` True

    it "length" $ do
      length (toDigitsRev number) `shouldBe` length (show number)

  describe "doubleSecond" $ do
    it "doubles every second number in the list" $ do
      doubleSecond [8,7,6,5] `shouldBe` [8,14,6,10]

    it "should be lazy" $ do
      take 4 (doubleSecond [1..]) `shouldBe` [1,4,3,8]

  describe "sumDigits" $ do
    it "calculates the sum of all INDIVIDUAL digits in the list" $ do
      sumDigits [8,14,6,10] `shouldBe` sum [8,(1+4),6,1]

    it "calculates the sum of all INDIVIDUAL digits in the list" $ do
      sumDigits [3,9,4,15,8] `shouldBe` sum [3,9,4,(1+5),8]

  describe "tells whether any input n :: Integer  where n >= 0  could be a valid credit card number" $ do
    it "works" $ do
      isValid number `shouldBe` True
