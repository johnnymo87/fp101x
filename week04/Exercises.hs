module Exercises where

factors :: Integral a => a -> [a]
factors a = [n | n <- [1..a-1], a `mod` n == 0]

perfects :: Integral a => a -> [a]
perfects a = [n | n <- [1..a], isPerfect n]
  where isPerfect num = sum (factors num) == num

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
-- scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
scalarproduct xs ys = sum (zipWith (*) xs ys)

divides :: Integral a => a -> a -> Bool
divides x y = x `mod` y == 0
