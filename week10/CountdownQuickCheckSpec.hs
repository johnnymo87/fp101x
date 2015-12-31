module CountdownQuickCheckSpec where

import Test.QuickCheck
import Countdown

instance Arbitrary Op where
  arbitrary = oneof [return Add, return Sub, return Mul, return Div]

instance Arbitrary Expr where
  arbitrary = sized genTree

factors a = [n | n <- [1..a], a `mod` n == 0]

                   -- op <- arbitrary
                   -- if op == Div
                   --   then do l <- genTree (size `div` 2)
                   --           r <- (elements (factors l))
                   --           return (App (op :: Op) l r)
                   --   else do l <- genTree (size `div` 2)
                   --           r <- genTree (size `div` 2)
                   --           return (App (op :: Op) l r)
genTree :: Int -> Gen Expr
genTree size
  | size > 0  = do
                   op <- arbitrary
                   l <- genTree ((size `div` 2) `mod` 4)
                   r <- genTree ((size `div` 2) `mod` 4)
                   return (App (op :: Op) l r)
  | otherwise = do
                   Positive i <- arbitrary
                   return (Val i)

-- sample' arbitrary :: IO [Expr]
-- fmap (Control.Monad.liftM eval) (sample' arbitrary :: IO [Expr])
