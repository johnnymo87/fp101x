module StateMonad where

import System.Random
import Control.Monad
import Control.Monad.Trans.State

-- instance Functor (State s) where
--     fmap f pr = state $ \ st ->
--        let (x, st') = runState pr st
--        in (f x, st')

rollDie :: State StdGen Int
-- rollDie = state $ randomR (1, 6)
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

rollNDice :: Int -> State StdGen [Int]
rollNDice n = replicateM n rollDie


-- rollDice :: StdGen -> ((Int, Int), StdGen)
-- rollDice g = ((n,m),g'')
--   where
--   (n,g')  = randomR (1,6) g
--   (m,g'') = randomR (1,6) g'

-- instance Monad (State s) where
--   -- return :: a -> State s a
--   return x = state (\ st -> (x, st))
--
--   -- (>>=) :: State s a -> (a -> State s b) -> State s b
--   pr >>= k = state $ \ st ->
--     let (x, st') = runState pr st
--     in runState (k x) st'
