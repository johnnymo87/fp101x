module Exercises where

fibs :: [Integer]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

fib :: Int -> Integer
-- fib n = last (take n fibs)
-- fib n = head (drop (n - 1) fibs)
fib n = fibs !! n

largeFib :: Integer
largeFib = head (dropWhile (<= 1000) fibs)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

repeatTree :: a -> Tree a
repeatTree x = Node t x t
  where t = repeatTree x
