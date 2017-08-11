module Ch21ExTraversableInstances where

import Data.Functor hiding (Constant)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  sequenceA (Identity a) = fmap Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> return $ Identity a)

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

testId = do
  let trigger = undefined :: Identity (Int, String, (String, String))
  quickBatch $ traversable trigger

-- 2.
newtype Constant a b = Constant { getConstant :: a } deriving Show

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  sequenceA (Constant a) = pure (Constant a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) x x' = getConstant x `eq` getConstant x'

testConstant = do
  let trigger = undefined :: (Constant Int (Int, String, [Int]))
  quickBatch $ traversable trigger

-- 3.
data Optional a = Yep a | Nada deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

  foldr _ b Nada = b
  foldr f b (Yep a) = f a b

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep a) = fmap Yep a

  traverse _ Nada = pure Nada
  traverse f (Yep a) = fmap Yep $ f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = arbitrary >>= (\a -> elements [(Yep a), Nada])

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

testMaybe = do
  let trigger = undefined :: (Optional (Int, String, [Int]))
  quickBatch $ traversable trigger

-- 4.

data List a = Cons a (List a) | Nil deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

  foldr _ b Nil = b
  foldr f b (Cons x xs) = foldr f (f x b) xs

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = arbitrary >>= (\a -> elements [(Cons a (Cons a Nil)), Nil])

instance (Eq a) => EqProp (List a) where (=-=) = eq

testList = do
  let trigger = undefined :: (List (Int, String, [Int]))
  quickBatch $ traversable trigger

-- 5.

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

  foldr f b' (Three a b c) = f c b'

instance Traversable (Three a b) where
  sequenceA (Three a b c) = fmap (Three a b) c

  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

testThree1 = do
  let trigger = undefined :: (Three Int String (Int, String, [Int]))
  quickBatch $ traversable trigger

-- 6.

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b `mappend` f b'

  foldr f x (Three' a b b') = (f b' (f b x)) -- Unless Monoid b => ...

instance Traversable (Three' a) where
  sequenceA (Three' a b b') = (Three' a) <$> b <*> b'

  traverse f (Three' a b b') = (Three' a) <$> (f b) <*> (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

testThree2 = do
  let trigger = undefined :: (Three' Int (Int, String, [Int]))
  quickBatch $ traversable trigger

-- S. TODO, no idea

-- Instances for Tree

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = (foldMap f l) `mappend` (f a) `mappend` foldMap f r

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = fmap Leaf a
  sequenceA (Node l a r) = Node <$> sequenceA l <*> a <*> sequenceA r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [(1, return Empty),
               (1, return (Leaf x)),
               (1, return (Node l x r))]

instance Eq a => EqProp (Tree a) where (=-=) = eq

testTree = do
  let trigger = undefined :: Tree (Int, String, [Int])
  quickBatch (traversable trigger)
