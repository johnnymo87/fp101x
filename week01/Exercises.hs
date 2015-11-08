module Exercises where

import Debug.Trace

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

qsort' :: Ord a => [a] -> [a]
qsort' [] = []
-- qsort' (x : xs) = qsort' larger ++ [x] ++ qsort' smaller
qsort' (x : xs) = reverse (reverse (qsort' smaller) ++ [x] ++ reverse (qsort' larger))
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
