{-# LANGUAGE InstanceSigs #-}

module Ch25ExComposeInstances where

import Control.Applicative (liftA2)

newtype Identity a = Identity a deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose f) (Compose a) = Compose $ liftA2 (<*>) f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- Identity Int => (Int -> Identity Int) -> Compose [] Maybe Int -> Identity Int
  -- 1. pattern match on fga, compose out, we're left with [Maybe Int] (3 layers: [], Maybe, Int)
  -- 2. compose two foldMap`s: this allows us to reduce two layers: [] and Maybe, so we touch actual values needed in f (a -> m), where m is a Monoid
  -- 3. apply composed foldMaps to a (a -> m) function and pattern-matched layers ([], Maybe, Int)
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- foldMap mappend (Compose [Nothing, Just "a", Just "b", Nothing]) []
-- > "ab"

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
--traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  -- traverse the structure to interact with the values, flip the types
  -- How?
  -- 1. Construct the Compose type
  -- 2. Since there are 2 layers of structure and a concrete type (f g a), we need to compose
  -- the traversal of the inner structures. Therefore we dispatch the function application to
  -- (traverse . traverse) and this takes care of traversal
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- traverse (fmap toUpper) [Just 'a', Just 'b']
-- > Just "ab"

