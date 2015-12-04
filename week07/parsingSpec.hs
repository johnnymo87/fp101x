module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (intercalate)

import Parsing

main :: IO ()
main = hspec $ do
  describe "item" $ do
    it "parses" $ do
      parse item "abc" `shouldBe` [('a',"bc")]

  describe "failure" $ do
    it "doesn't parse" $ do
      parse failure "abc" `shouldBe` ([] :: [(Char, String)])

  describe "character parsing" $ do
    describe "digit" $ do
      it "succeeds if the parsed character is a digit" $ do
        parse digit "123" `shouldBe` [('1',"23")]

      it "fails if the parsed character is not a digit" $ do
        parse digit "abc" `shouldBe` ([] :: [(Char, String)])

    describe "lower" $ do
      it "succeeds if the parsed character is lowercase" $ do
        parse lower "abc" `shouldBe` [('a',"bc")]

      it "fails if the parsed character is not lowercase" $ do
        parse lower "ABC" `shouldBe` ([] :: [(Char, String)])

    describe "upper" $ do
      it "succeeds if the parsed character is uppercase" $ do
        parse upper "ABC" `shouldBe` [('A',"BC")]

      it "fails if the parsed character is not uppercase" $ do
        parse upper "abc" `shouldBe` ([] :: [(Char, String)])

    describe "alphanum" $ do
      it "succeeds if the parsed character is alphanumeric" $ do
        parse upper "ABC" `shouldBe` [('A',"BC")]

      it "fails if the parsed character is not alphanumeric" $ do
        parse upper "   " `shouldBe` ([] :: [(Char, String)])

    describe "char" $ do
      it "succeeds if the parsed character is a certain character" $ do
        parse (char 'a') "abc" `shouldBe` [('a',"bc")]

      it "succeeds if the parsed character is not a certain character" $ do
        parse (char 'a') "xyz" `shouldBe` ([] :: [(Char, String)])

    describe "string" $ do
      it "succeeds if the string is at the front" $ do
        parse (string "ab") "abcd" `shouldBe` [("ab","cd")]

      it "fails if the string is not at the front" $ do
        parse (string "ab") "cdab" `shouldBe` ([] :: [(String, String)])

  describe "integer parsing" $ do
    describe "nat" $ do
      it "parses digits" $ do
        parse nat "123" `shouldBe` [(123,"")]

      it "until it encounters a non-digit" $ do
        parse nat "123abc" `shouldBe` [(123,"abc")]

      it "fails if there are no digits at the beginning of the string" $ do
        parse nat "abc" `shouldBe` ([] :: [(Int, String)])

    describe "int" $ do
      it "parses a digit" $ do
        parse int "0077" `shouldBe` [(77,"")]

      it "parses a negative digit" $ do
        parse int "-0077" `shouldBe` [(-77,"")]

  describe "ignored elements" $ do
    describe "space" $ do
      it "consumes spaces" $ do
        parse space "   hi " `shouldBe` [((),"hi ")]

    describe "comment" $ do
      it "consumes comments until the newline" $ do
        parse comment "-- hello world\nhi" `shouldBe` [((),"\nhi")]

      it "fails with non-comments" $ do
        parse comment "hello world" `shouldBe` ([] :: [((), String)])

  describe "expr" $ do
    it "works for subtraction" $ property $
      let spaces a = take a $ repeat ' '
      in \x y ->
        forAll (choose (0, 9)) $ \z ->
           x - y == (
             fst $
             head $ (
               parse expr (
                 intercalate (spaces z) [" ", show x, "-", show y, " "]
               )
             )
           )
