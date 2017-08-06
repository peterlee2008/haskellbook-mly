module Ch18ExMonadInstances where

import Control.Monad
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--
-- 1.
--
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) (NopeDotJpg) _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

testNope = do
  let triggerNope = undefined :: Nope (Int, String, Int)
  quickBatch $ functor triggerNope
  quickBatch $ applicative triggerNope
  quickBatch $ monad triggerNope

--
-- 2.
--

data PffEither b a = -- flipped either, Right is part of the functorial structure
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PffEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PffEither b) where
  pure x = Left' x

  (<*>) (Left' f) (Left' b) = Left' $ f b
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b

instance Monad (PffEither b) where
  return = pure

  (>>=) (Right' b) _ = Right' b
  (>>=) (Left' a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PffEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ (Left' a)
             , (Right' b)
             ]

instance (Eq a, Eq b) => EqProp (PffEither b a) where (=-=) = eq

testPffEither = do
  let triggerPffEither = undefined :: PffEither (Int, String, Bool) (Int, String, Bool)
  quickBatch $ functor triggerPffEither
  quickBatch $ applicative triggerPffEither
  quickBatch $ monad triggerPffEither


--
-- 3.
--

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure

  (>>=) (Identity a) k = k a

{--
-- Full of sugarz
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)
--}

-- no sugarz
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> return (Identity a))

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

testIdentity = do
  let triggerId = undefined :: Identity (Int, String, Bool)
  quickBatch $ functor triggerId
  quickBatch $ applicative triggerId
  quickBatch $ monad triggerId

--
-- 4.
--

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- Copied from Ch17
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) $ fmap f rest

instance Applicative List where
  pure x = Cons x Nil

  (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)
  (<*>) Nil xs = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

--

instance Monad List where
  return = pure

  (>>=) (Cons x xs) k = (k x) `append` (join $ k <$> xs)
  (>>=) Nil _ = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = arbitrary >>= (\a -> elements [(Cons a Nil), Nil])

instance (Eq a) => EqProp (List a) where (=-=) = eq

testList = do
  let triggerList = undefined :: List (Int, String, Bool)
  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList
