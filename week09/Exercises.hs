module Exercises where
import Data.List
import Data.Char
-- Q: Why do we need to import unsafeCoerce in exercise 0? It is not used in any possible answer option.
-- A: My guess is, it's to be able to use a common supplied-code snippet for Exercise 0 and Exercise 1.
-- import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving Show

natToInteger :: Nat -> Integer
