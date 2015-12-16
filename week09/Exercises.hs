module Exercises where
import Data.List
import Data.Char
-- Q: Why do we need to import unsafeCoerce in exercise 0? It is not used in any possible answer option.
-- A: My guess is, it's to be able to use a common supplied-code snippet for Exercise 0 and Exercise 1.
-- import Hugs.IOExts (unsafeCoerce)
import Unsafe.Coerce (unsafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)
         -- Show: to support the meta-programming defining of natToInteger
         -- Eq: to support `shouldBe` for integerToNat

natToInteger :: Nat -> Integer
-- natToInteger Zero = 0
-- natToInteger (Succ n)  = natToInteger n + 1

-- has "cannot construct infinite type" errors if defined without head
-- natToInteger = head . func
--   where func Zero = [0]
--         func (Succ n) = [sum [x | x <- (1 : func n)]]

-- meta-programming?
natToInteger n = genericLength [c | c <- show n, c == 'S']

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))

add :: Nat -> Nat -> Nat
-- add Zero n = n
-- add (Succ m) n = Succ (add n m)
add m Zero = m
add m (Succ n) = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data Tree = Leaf Integer
          | Node Tree Integer Tree
          -- deriving (Show, Eq)

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  -- -- sorted asc
  -- | m == n = True
  -- | m < n = occurs m l
  -- | otherwise = occurs m r
  -- -- sorted desc
  -- | m == n = True
  -- | m > n = occurs m l
  -- | otherwise = occurs m r
  = case compare m n of
        -- sorted asc
        LT -> occurs m l
        EQ -> True
        GT -> occurs m r
        -- sorted desc
        -- LT -> occurs m r
        -- EQ -> True
        -- GT -> occurs m l

data Tree' = Leaf' Integer
          | Node' Tree' Tree'
          deriving (Show, Eq)

leaves :: Tree' -> Integer
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' -> Bool
balanced (Leaf' _) = True
balanced (Node' l r)
  = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [Integer] -> Tree'
balance [x] = Leaf' x
balance xs = Node' (balance ys) (balance zs)
  where (ys, zs) = halve xs

data Expr = Val Int | Add Expr Expr deriving (Show)
