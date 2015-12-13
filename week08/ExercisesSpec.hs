module ExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Exercises

main :: IO ()
main = hspec $ do
  describe "putStr'" $ do
    it "works" $ do
      putStr' "" `shouldReturn` ()
      putStr' "hello world" `shouldReturn` ()

  describe "putStrLn'" $ do
    it "works" $ do
      putStrLn' "" `shouldReturn` ()
      putStrLn' "hello world" `shouldReturn` ()
