module Ch17ExZipListApplicative where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = (Cons x (take' (n - 1) xs))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) $ fmap f rest

instance Applicative List where
  pure a = (Cons a Nil)
  (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)
  (<*>) Nil xs = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 100 l
          ys' = let (ZipList' l) = ys
                in take' 100 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ zipListZipWith id fs xs

repeat' :: a -> List a
repeat' x = xs where
  xs = Cons x $ repeat' x

zipListZipWith :: (a -> b -> c) -> List a -> List b -> List c
zipListZipWith f (Cons a as) (Cons b bs) = Cons (f a b) $ zipListZipWith f as bs
zipListZipWith _ Nil _ = Nil
zipListZipWith _ _ Nil = Nil

z = ZipList' (Cons (+9) (Cons (*2) (Cons (+8) Nil)))
z' = ZipList' (Cons 1 (Cons 1 (Cons 1 Nil)))
