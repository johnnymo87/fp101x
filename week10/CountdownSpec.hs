module CountdownSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Countdown

main :: IO ()
main = hspec $ do
  describe "quickcheck Generators" $ do
    it "builtin generator for positives" $
      let prop_positive (Positive x) = (\a -> a > 0) x
      in property (prop_positive :: Positive Integer -> Bool)

    let positives = do -- Pick an arbitrary integer:
                       x <- arbitrary
                       -- Make it positive, if necessary:
                       if (x == 0)
                         then return 1
                       else if (x < 0)
                         then return (-x)
                       else
                         return x

    it "hand-made generator for positives" $ property $
      forAll (positives :: Gen Integer) $ \x -> (\a -> a > 0) x

    it "hand-made generator for positives using suchThat" $ property $
      let p = (\a -> a > 0) :: Int -> Bool
      in forAll (suchThat arbitrary p) $ \x -> p x

  describe "Expr" $ do
    it "Val Int" $ do
      Val 1 `shouldBe` Val 1

    it "App Op Expr Expr" $ do
      App Add (Val 1) (Val 2) `shouldBe` App Add (Val 1) (Val 2)

    describe "Add" $ do
      describe "valid" $ do
        it "is always true" $ property $
          \x y -> valid Add x y

    describe "Sub" $ do
      describe "valid" $ do
        it "is true when x > y" $ property $
          \x y -> (\a b -> a > b) x y ==> valid Sub x y

    describe "Mul" $ do
      describe "valid" $ do
        it "is always true" $ property $
          \x y -> valid Mul x y

    describe "Div" $ do
      let factors a = [n | n <- [1..a], a `mod` n == 0]

      describe "valid" $ do
        it "is true when x `mod` y == 0" $ property $
          forAll (choose (1, 30)) $ \x ->
          forAll (elements (factors x)) $ \y ->
            valid Div x y
