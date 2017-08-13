{-# LANGUAGE InstanceSigs #-}

module Ch22ExReadingComprehension where

-- 1. Write liftA2

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2.
newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks ra = Reader ra

-- 3.

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- We need to pass the "r's" to both Readers. It's just "dual application",
-- but since each Reader is r -> a, they take one extra argument.
