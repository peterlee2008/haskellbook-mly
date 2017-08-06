module Ch18ExFunctionsWithMethods where

import Control.Applicative
import Control.Monad

-- 1.
j :: Monad m => m (m a) -> m a
j = join


-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftA

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4. TODO
-- a :: Monad m => m a -> m (a -> b) -> m b
-- I'm lost here...

-- 5. TODO
--

-- 6. TODO
--
