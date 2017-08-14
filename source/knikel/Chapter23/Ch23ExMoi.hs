module Ch23ExMoi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, b) = (g s)
                               in (f a, b)
-- OK, this was hard. Why it works? Well...
-- This is how I understand it...
-- The result of runState should be (a, s), which is what \s -> lambda returns
-- What we do in that lambda is as follows:
-- 1. We apply g function (~current state) to s (~next state).
--    The output will be a tuple of a value a and State s ~ (a, s)
-- 2. with let (a, b) we pattern match these values
-- 3. return a new tuple where the f passed to fmap is applied to the value
--    alongside the state.

instance Applicative (Moi s) where
  pure a = Moi (\s -> (a, s))

  (Moi f) <*> (Moi g) = Moi $ \s -> let (fab, s') = f s
                                        (a, b) = g s'
                                    in (fab a, b)

-- We're doing two consecutive function applications with f s, then g s'

instance Monad (Moi s) where
  return = pure

  (Moi f) >>= g = Moi $ \s -> let (a, s) = f s
                              in runMoi (g a) s
-- deconstruct the new state from (f s) application
-- smush by runMoi (g a) s, where s is the ~new state~, and (g a) is the
-- sequenced state function applied to state from (f s)
