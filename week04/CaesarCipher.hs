module CaesarCipher where

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let((let2int c + n) `mod` 26)
  | isAlpha c = toUpper $ shift n $ toLower c
  | otherwise = c

encode :: Int -> String -> String
-- This is equivalent
-- encode n xs = [shift n x | x <- xs]
encode n xs = xs >>= \x -> return $ shift n x
