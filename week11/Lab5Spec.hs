module Lab5Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lab5

main :: IO ()
main = hspec $ do
  describe "writeAction" $ do
    it "applies the algorithm to the inputs" $ do
      show (Fork (writeAction "Hello\n") (writeAction "fp101x")) `shouldBe` "fork atom atom"

