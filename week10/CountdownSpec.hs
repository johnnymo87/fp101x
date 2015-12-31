module CountdownSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Data.List (permutations)

import Countdown
import CountdownQuickCheckSpec

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

    -- let expression = do -- Pick an arbitrary operand:
    --                     x <- arbitrary
    --                     return x
    -- it "custom Arbitrary instance for Op" $ do
    --   elem (head (show expression)) "+_*/" `shouldBe` True

  describe "Expr" $ do
    it "Val Int" $ do
      Val 1 `shouldBe` Val 1

    it "App Op Expr Expr" $ do
      App Add (Val 1) (Val 2) `shouldBe` App Add (Val 1) (Val 2)

    describe "Add" $ do
      describe "valid" $ do
        it "is always true" $ property $
          \x y -> valid Add x y

      describe "apply" $ do
        it "is addition" $ property $
          \x y -> apply Add x y == ((+) x y)

    describe "Sub" $ do
      describe "valid" $ do
        it "is true when x > y" $ property $
          \x y -> (\a b -> a > b) x y ==> valid Sub x y

      describe "apply" $ do
        it "is subtraction" $ property $
          \x y -> apply Sub x y == ((-) x y)

    describe "Mul" $ do
      describe "valid" $ do
        it "is always true" $ property $
          \x y -> valid Mul x y

      describe "apply" $ do
        it "is multiplication" $ property $
          \x y -> apply Mul x y == ((*) x y)

    describe "Div" $ do
      let factors a = [n | n <- [1..a], a `mod` n == 0]

      describe "valid" $ do
        it "is true when x `mod` y == 0" $ property $
          forAll (choose (1, 30)) $ \x ->
          forAll (elements (factors x)) $ \y ->
            valid Div x y

      describe "apply" $ do
        it "is division" $ property $
          forAll (choose (1, 30)) $ \x ->
          forAll (elements (factors x)) $ \y ->
            apply Div x y == div x y

  describe "values" $ do
    it "extracts all the numbers" $ do
      values (Val 1) `shouldBe` [1]
      values (App Add (Val 1) (Val 2)) `shouldBe` [1,2]

  describe "eval" $ do
    it "evaluates the expression" $ do
      eval (Val 1) `shouldBe` [1]
      eval (App Add (Val 1) (Val 2)) `shouldBe` [3]

  describe "subs" $ do
    it "recursively maps each element over lists of the remaining elements" $ do
      subs [1..3] `shouldBe` [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

  describe "interleave" $ do
    it "inserts the element at each position in the list" $ do
      interleave 7 [1..3] `shouldBe` [[7,1,2,3],[1,7,2,3],[1,2,7,3],[1,2,3,7]]

  describe "interleave" $ do
    it "returns all permutations (orderings) of the list" $ do
      perms [1..3] `shouldBe` [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

  describe "choices" $ do
    it "returns all possible ways of choosing zero or more elements from the list" $ do
      choices [1..2] `shouldBe` [[1,2],[2,1],[1],[2],[]]

    it "can also be defined as follows" $ do
      let choices' xs = [zs | ys <- subs xs, zs <- perms ys]
      choices [1..3] `shouldBe` choices' [1..3]

    -- it "can also be defined as follows" $ property $
    --   let choices' xs = [zs | ys <- subs xs, zs <- perms ys]
    --   in \xs -> choices (xs :: [Int]) == choices' xs

  describe "solution" $ do
    it "returns true if an expression is using the given numbers and computes to the desired number" $ do
      solution (App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1))) [1,3,7,10,25,50] 765 `shouldBe` True

  describe "split" $ do
    it "returns all splits of a list, excluding the empty list" $ do
      split [1..4] `shouldBe` [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

  describe "exprs" $ do
    it "returns all possible expressions" $ do
      exprs [1,2] `shouldBe` map (\o -> (App o (Val 1) (Val 2))) [Add, Sub, Mul, Div]

  describe "combine" $ do
    it "returns all possible expressions for two values" $ do
      combine (Val 1) (Val 2) `shouldBe` map (\o -> (App o (Val 1) (Val 2))) [Add, Sub, Mul, Div]

  describe "solutions" $ do
    it "returns all possible solutions" $ do
      solutions [1,2] 3 `shouldBe` [(App Add (Val 1) (Val 2)),(App Add (Val 2) (Val 1))]

  describe "removeOne" $ do
    it "removes the first occurence of a given element from a list" $ do
      let {
            removeOne x [] = [];
            removeOne x (y : ys)
              | x == y = ys
              | otherwise = y : removeOne x ys
          }
      removeOne 2 [1,2,3,2,1] `shouldBe` [1,3,2,1]

  describe "isChoice" $ do
    describe "isChoice xs ys checks whether all elements in xs are present in ys" $ do
      let {
            removeOne x [] = [];
            removeOne x (y : ys)
              | x == y = ys
              | otherwise = y : removeOne x ys
          }
      let {
            isChoice [] _  = True;
            isChoice xs [] = False;
            isChoice (x:xs) ys
              | elem x ys = isChoice xs (removeOne x ys)
              | otherwise = False
          }

      it "is True when all are" $ do
        isChoice [1,2] [1,2,3] `shouldBe` True

      it "is False when not all are" $ do
        isChoice [1,2,3] [1,2] `shouldBe` False
