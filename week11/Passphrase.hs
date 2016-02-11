module Passphrase where

import Data.Char (isAlpha, isNumber, isPunctuation)
import Control.Monad
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- instance Show (MaybeT m a) where
--     show (MaybeT m) = "hello"

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                             Nothing    -> return Nothing
                             Just value -> runMaybeT $ f value

instance Monad m => MonadPlus (MaybeT m) where
  mzero     = MaybeT $ return Nothing
  mplus x y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> runMaybeT y
                               Just _     -> return maybe_value

instance MonadTrans MaybeT where
  lift = MaybeT . (liftM Just)

getValidPassphrase :: MaybeT IO String
getValidPassphrase = do s <- lift getLine
                        guard (isValid s)
                        return s

-- runMaybeT askPassphrase
askPassphrase :: MaybeT IO ()
askPassphrase = do lift $ putStrLn "Insert your new passphrase:"
                   value <- getValidPassphrase
                   lift $ putStrLn "Storing in database..."

-- import Data.Maybe (isJust)
--
--
-- getPassphrase :: IO (Maybe String)
-- getPassphrase = do s <- getLine
--                    if isValid s then return $ Just s
--                                 else return Nothing
--
-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s
--
-- askPassphrase :: IO ()
-- askPassphrase = do putStrLn "Insert your new passphrase:"
--                    maybe_value <- getPassphrase
--                    if isJust maybe_value
--                      then do putStrLn "Storing in database..." -- do stuff
--                      else putStrLn "Passphrase invalid."
