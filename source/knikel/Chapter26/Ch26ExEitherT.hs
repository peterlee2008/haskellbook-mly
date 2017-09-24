module Ch26ExEitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

-- 1.
instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

-- 2.
instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x

  (EitherT fab) <*> (EitherT mea) =
    EitherT $ (<*>) <$> fab <*> mea

-- 3.
instance (Monad m) => Monad (EitherT e m) where
  return = pure

  EitherT mea >>= f = EitherT $ do
    ea <- mea
    case ea of
      (Left ea) -> return $ Left ea
      (Right ea) -> runEitherT $ f ea

-- 4.
swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

-- 5.
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g x= do
  ab <- runEitherT x
  case ab of
    (Left ab) -> f ab
    (Right ab) -> g ab
