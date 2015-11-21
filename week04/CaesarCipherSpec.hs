module CaesarCipherSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import CaesarCipher

main :: IO ()
main = hspec $ do
  describe "let2int" $ do
    it "returns the alphabetical index of a letter, with 'a' being 0" $ do
      let2int 'f' `shouldBe` 5

  describe "int2let" $ do
    it "returns the letter by its alphabetical index, with 'a' being 0" $ do
      int2let 5 `shouldBe` 'f'

  describe "shift" $ do
    describe "lowercase letters" $ do
      it "shifts a letter by the given amount" $ do
        shift 5 'a' `shouldBe` 'f'

      it "wraps around" $ do
        shift 31 'a' `shouldBe` 'f'

    describe "uppercase letters" $ do
      it "shifts a letter by the given amount" $ do
        shift 5 'A' `shouldBe` 'F'

      it "wraps around" $ do
        shift 31 'A' `shouldBe` 'F'

    describe "characters that are not letters" $ do
      it "passes through" $ do
        shift 31 ' ' `shouldBe` ' '

  describe "encode" $ do
    it "shifts an entire string by the given amount" $ do
      encode 5 "hello" `shouldBe` "mjqqt"

    it "works with uppercase letters" $ do
      let before = "Think like a Fundamentalist Code like a Hacker"
      let after = "Guvax yvxr n Shaqnzragnyvfg Pbqr yvxr n Unpxre"
      encode 13 before `shouldBe` after


