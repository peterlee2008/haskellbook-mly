module Ch20ChapterExercises where

import Data.Monoid ((<>))

-- 1.

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

-- OK, so we have few things in play here:
-- (a -> Bool)
-- Foldable a
-- Monoid (Applicative  a)
-- the way to go about solving this is:
-- check if (f a) -> True on the currently folded element
-- wrap this in the structure through pure (Applicative constraint when returned)
-- otherwise, return mempty (Monoid constraint when returned)
-- subsequent `mempty` will be mappended during fold, since we delegate to the Monoid
-- instance.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = foldMap (\a -> if f a then pure a else mempty) xs
