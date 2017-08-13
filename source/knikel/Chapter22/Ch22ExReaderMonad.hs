{-# LANGUAGE InstanceSigs #-}
module Ch22ExReaderMonad where

import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a

  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb = join $ Reader $ \r -> aRb (ra r)
-- (ra r) becomes a
-- once aRb gets a, it becomes Reader r b
-- join gets Reader (Reader r b)
-- we smush the structure to get Reader r b

-- I get the types and thinking behind it, but I have no idea when
-- or how I would use that yet.
