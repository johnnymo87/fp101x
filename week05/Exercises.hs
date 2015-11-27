module Exercises where

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs

merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs
