module Ch17ChapterExercises where

import Control.Applicative
import Data.Monoid

-- Specialize the types

-- 1. []
pure1 :: a -> [a]
pure1 = undefined

ap1 :: [(a -> b)] -> [a] -> [b]
ap1 = undefined

-- 2. IO
pure2 :: a -> IO a
pure2 = undefined

-- 3. (,) a
-- TODO; Not sure

-- 4. (->) e
-- TODO; Not sure

-- Write Applicative instances
-- TODO: Use checkers to validate

-- 1.
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')

-- 2.
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

-- 3.
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

-- 4.
data Three' a b  = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f f') (Three' a' x x') = Three' (a <> a') (f x) (f' x')

-- 5.
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c f) (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)

-- 6.
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' d) = Four' a a' a'' (f d)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a a' a'' f) (Four' b b' b'' x) = Four' (a <> b) (a' <> b') (a'' <> b'') (f x)

-- Combinations, see Ch17Combinations
