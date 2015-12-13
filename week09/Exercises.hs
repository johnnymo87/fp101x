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

