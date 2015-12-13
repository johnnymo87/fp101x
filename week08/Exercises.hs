module Exercises where

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putChar '\n'

getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
         '\n' -> return xs
         _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()
interact' f
  = do input <- getLine'
       putStrLn' (f input)

-- sequence_' [(putStrLn' "How are you?"), (putStrLn' "Great!")]]
sequence_' :: Monad m => [m a] -> m ()
-- sequence_' [] = return ()
-- sequence_' (m : ms) = (foldl (>>) m ms) >> return ()
--
-- sequence_' (m : ms) = m >> sequence_' ms
sequence_' ms = foldr (>>) (return ()) ms

-- sequence' [(interact' reverse), (interact' (fmap Data.Char.toUpper))]
sequence' :: Monad m => [m a] -> m [a]
-- sequence' [] = return []
-- sequence' (m : ms)
--   = m >>=
--       \a ->
--         do as <- sequence' ms
--            return (a : as)
-- sequence' ms = foldr func (return []) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc
--       = do x <- m
--            xs <- acc
--            return (x : xs)
sequence' [] = return []
sequence' (m : ms)
  = do a <- m
       as <- sequence' ms
       return (a : as)

-- mapM' (get) ["bob", "cat", "dad"]
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM' f as = sequence' (map f as)
mapM' f [] = return []
mapM' f (a : as)
  = f a >>= \b -> mapM' f as >>= \bs -> return (b : bs)
-- mapM' f [] = return []
-- mapM' f (a : as)
--   = f a >>=
--       \ b ->
--         do bs <- mapM' f as
--            return (b : bs)

-- filterM' (\x -> if (Data.Char.isAlpha x) then [True] else []) "abc"
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys

foldl' _ a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

-- foldLeftM (\a b -> if b /= 0 then Just (a / b) else Nothing) 1000 [2,2,2]
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM _ a [] = return a
foldLeftM f a (x:xs) = f a x >>= \fax -> foldLeftM f fax xs

foldr' f a [] = a
foldr' f a (x:xs) = foldr' f (f x a) xs

-- foldLeftM (\a b -> if b /= 0 then Just (a / b) else Nothing) 1000 [2,2,2]
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM _ a [] = return a
foldRightM f a (x:xs) = f x a >>= \fxa -> foldRightM f fxa xs

-- liftM (map Data.Char.toUpper) getLine
liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f m
--   = do x <- m
--        return (f x)
liftM f m = m >>= \m' -> return (f m')
