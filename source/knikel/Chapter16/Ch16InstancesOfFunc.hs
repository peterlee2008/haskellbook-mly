module Ch16InstancesOfFunc where

-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a


-- 2.
data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-- 3.
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b


-- 4.
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

-- 5.
data Three' a b  = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6.
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7.
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' d) = Four' a a' a'' (f d)

-- 8.
-- It's impossible to implement a Functor for
-- data Trivial - Trivial
-- it has kind * (no type argument which Functor requires)
