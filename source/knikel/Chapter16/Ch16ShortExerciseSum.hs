module Ch16ShortExerciseSum where

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

-- 1.
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- 2.
-- The (First a) is part of the structure, since Functor instance expects kind of * -> *, where Sum a b has kind * -> * -> *.
-- The same applies to Either's Left.
