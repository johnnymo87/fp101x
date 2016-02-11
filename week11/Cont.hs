module Cont where

import Control.Monad.Cont

-- https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style

-- We assume some primitives add and square for the example:

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

-- We assume CPS versions of the add and square primitives,
-- (note: the actual definitions of add_cps and square_cps are not
-- in CPS form, they just have the correct type)

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

-- pythagoras_cps 2 3 id
pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
 square_cps x $ \x_squared ->
 square_cps y $ \y_squared ->
 add_cps x_squared y_squared $ k

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
  f_cps x $ \fx ->
  f_cps fx $ \ffx ->
  f_cps ffx $ k

-- thrice_cps reverse_cps "hello" id
reverse_cps :: [a] -> (([a] -> r) -> r)
reverse_cps a = \k -> k (reverse a)

-- suspends a computation in a continuation
sus :: (a -> a) -> a -> ((a -> a) -> a)
sus f a = \k -> k (f a)

-- takes a continuation, a function that advances the continuation by one,
-- and returns the advanced continuation
-- i.e. ChainCPS ($ "hello") reverse_cps id
--      ChainCPS (sus reverse "hello") (sus reverse) id
--       :: m a -> (a -> m b) -> m b
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS c f_cps = \k ->
  c $ \x ->
  f_cps x $ k


-- wrap :: r -> ((a -> a) -> r)
-- wrap' :: t1 -> ((t1 -> t) -> t)
-- wrap' = \x -> \k -> k x
-- wrap' = \k -> \x -> x k

unwrap :: ((a -> a) -> r) -> r
-- unwrap k = k id
-- unwrap = \k -> k id
unwrap = \k -> k (\x -> x)

-- unwrap' :: ((a -> r) -> r) -> r
-- unwrap' k = k id
-- unwrap' = \k -> k (\x -> k)

-- runCont (Cont (sus reverse "hello")) id
-- newtype Cont r a = Cont { runCont :: (a -> r) -> r }
--
-- instance Monad (Cont r) where
--   return x = Cont ($ x)
--   s >>=  f = Cont $ \c -> runCont s $ \x -> runCont (f x) c

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (add x y)

-- runCont (square_cont 2 >>= \x -> add_cont 3 x) id
square_cont :: Int -> Cont r Int
square_cont x = return (square x)

-- trivial example of using callCC
squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n ^ 2)

-- runCont (pythagoras_cont 2 3) id
pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
    x_squared <- squareCCC x
    y_squared <- square_cont y
    add_cont x_squared y_squared

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty"
    return (show $ y - 4)

bar :: Char -> String -> Cont r Int
bar c s = do
    msg <- callCC $ \k -> do
        let s0 = c : s
        when (s0 == "hello") $ k "They say hello."
        let s1 = show s0
        return ("They appear to be saying " ++ s1)
    return (length msg)

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
    err <- callCC $ \notOk -> do
        when (y == 0) $ notOk "Denominator 0"
        ok $ x `div` y
    handler err

tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryCont c h = callCC $ \ok -> do
    err <- callCC $ \notOk -> do
        x <- c notOk
        ok x
    h err

data SqrtException = LessThanZero deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
    ln <- lift (putStr "Enter a number to sqrt: " >> readLn)
    when (ln < 0) (throw LessThanZero)
    lift $ print (sqrt ln)

main_sqrtIO = runContT (tryCont sqrtIO (lift . print)) return
