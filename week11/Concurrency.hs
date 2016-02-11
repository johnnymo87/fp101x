module Concurrency where

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. -1
-- http://www.seas.upenn.edu/~cis552/lectures/Concurrency.html
-- ===================================

-- Actions run piece-by-piece until the stop
writeAction :: String -> Action
writeAction ""     = Stop
writeAction (c:cs) = Atom $ do
  putChar c
  return $ writeAction cs

prog :: Action
prog = Fork (writeAction "Hello\n") (writeAction "fp101x\n")

other_prog = Fork (writeAction "Goodbye\n") (writeAction "for now\n")

sched :: [Action] -> IO ()
sched [] = return ()
-- in the case of prog, this prints 'H', and returns "ello", which sched puts at
-- the end of the queue.
sched (Atom io : cs) = io >>= \a -> sched (cs ++ [a])
sched (Fork a1 a2 : cs) = sched (cs ++ [a1, a2])
sched (Stop: cs) = sched cs

-- *Lab5> sched [prog]
-- Hfepl1l0o1
-- x

-- Factor out the "last step". In `writeAction`, the last action is `Stop`.
-- Here, the last action is paramter `k`.
writeComputation :: String -> Action -> Action
writeComputation []     k = k
writeComputation (c:cs) k = Atom $ do
  putChar c
  return $ writeComputation cs k

sequenceComputation :: (Action -> Action)
                    -> (Action -> Action)
                    -> (Action -> Action)
sequenceComputation m n = \k -> m (n k)

hellofp101x = writeComputation "Hello " `sequenceComputation`
              writeComputation "fp101x\n"

-- *Lab5> sched [hellofp101x Stop]
-- Hellofp101x*Lab5>

-- suspend :: (a -> Action) -> a -> ((a -> Action) -> Action)
-- suspend f a = \k -> Atom $ f a >>= \c -> return $ k c
-- suspend f a = \k -> (f a) >>= \c -> return $ k c
-- suspend f a = \k -> Atom $ k (f a)

-- suspend :: a -> (a -> IO Action) -> Action
-- suspend a = \k -> Atom $ k a

readComputation :: (Char -> Action) -> Action
readComputation k = Atom $ getChar >>= \c -> return $ k c

-- other_prog = Fork (writeAction "Goodbye\n") (writeAction "for now\n")
-- read_prog = 

-- chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
-- chainCPS c f_cps = \k ->
--   c $ \x ->
--   f_cps x $ k

sequenceComp :: ((a -> Action) -> Action)
              -> (a -> ((b -> Action) -> Action))
              -> ((b -> Action) -> Action)
sequenceComp m f = \k -> m (\ v -> f v k)

type CM a = (a -> Action) -> Action

sequenceCompM :: CM a -> (a -> CM b) -> CM b
sequenceCompM m f = \k -> m (\v -> f v k)

writeCompM :: String -> CM ()
writeCompM str = \k -> writeComputation str $ k ()

ex = sequenceCompM readComputation (\c -> writeCompM [c])

returnCompM :: a -> CM a
returnCompM x = \k -> k x
