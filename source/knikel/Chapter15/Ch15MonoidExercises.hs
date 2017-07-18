module Ch15MonoidExercises where

import Data.Semigroup
import Data.Monoid hiding ((<>))
import Test.QuickCheck

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

-- 1
data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty  = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity x' = Identity $ x <> x'

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoStringAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    elements [ BoolConj True
             , BoolConj False
             ]
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj True) = BoolDisj True
  true@(BoolDisj True) <> _ = true
  _ <> true@(BoolDisj True) = true
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj True) (BoolDisj True) = BoolDisj True
  mappend true@(BoolDisj True) _ = true
  mappend _ true@(BoolDisj True) = true
  mappend _ _ = mempty

instance Arbitrary BoolDisj where
  arbitrary = do
    elements [ BoolDisj True
             , BoolDisj False
             ]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6
-- FIXME: Add CoArbitrary and QuickCheck tests
-- This makes more sense now, but I can't explain the Semigroup b constraint in the Monoid instance

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ (f <> g)

instance Semigroup b => Monoid (Combine a b) where
  mempty = mempty
  mappend = (<>)

-- 7
-- FIXME: Add CoArbitrary and QuickCheck tests

newtype Comp a = Comp (a -> a)

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance Monoid (Comp a) where
  mempty = mempty
  mappend = (<>)

-- 8
-- FIXME: Too hard.

main = do
  -- 1
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity:: Trivial -> Bool)
  -- 2
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  -- 3
  quickCheck (semigroupAssoc :: TwoStringAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  -- 4
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  -- 5
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  -- 6 FIXME
  -- 7 FIXME
  -- 8 FIXME
