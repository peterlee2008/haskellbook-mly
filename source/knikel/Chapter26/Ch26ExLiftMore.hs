module Ch26ExLiftMore where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Reader


newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

-- runEitherT $ (lift (Just (1 :: Integer)) :: EitherT String Maybe Integer)

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift c = StateT $ \s ->
    c >>= (\x -> return (x,s))
