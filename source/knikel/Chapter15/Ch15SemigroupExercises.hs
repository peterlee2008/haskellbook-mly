module Ch15SemigroupExercises where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 1.
instance Semigroup Trivial where
  _ <> _ = Trivial

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity x') = Identity $ x <> x'

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoStringStringAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeStringOrderingStringAssoc = Three String Ordering String -> Three String Ordering String -> Three String Ordering String -> Bool

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourStringOrderingOrderingString = Four String Ordering Ordering String -> Four String Ordering Ordering String -> Four String Ordering Ordering String -> Bool

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    elements [ BoolConj True
             , BoolConj False
             ]

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _             = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    elements [ BoolDisj True
             , BoolDisj False
             ]
-- 8.
data Or a b = Fst a
  | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd b) = Snd b
  _ <> (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ Fst a
             , Snd b
             ]

type OrStringString = Or String String -> Or String String -> Or String String -> Bool

-- 9.
{--

WARN:
I'm not sure WHY this works. I stumbled upon this solution while trying to have
(Semigroup a, Semigroup b) constraint. This was failing, and removing the Semigroup a
seems to solve it. Still though, I don't know why this works and I have no clue how to
write the CoArbitrary for QuickCheck.

--}
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ f <> g

-- 10.
-- This makes more sense than the above
newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f <> g

-- 11.
data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure' a <> Failure' a' = Failure' $ a <> a'
  Success' a <> _ = Success' a
  _ <> Success' b = Success' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ Failure' a
             , Success' b
             ]

type ValidationSS = Validation String String -> Validation String String -> Validation String String -> Bool

-- 12.

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success' b) <> AccumulateRight (Success' b') = AccumulateRight $ Success' (b <> b')
  AccumulateRight (Success' b) <> _ = AccumulateRight $ Success' b
  _ <> AccumulateRight (Success' b') = AccumulateRight $ Success' b'
  _ <> failure = failure

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ AccumulateRight $ Failure' a
             , AccumulateRight $ Success' b
             ]

type AccumulateRightValidation = AccumulateRight  String String -> AccumulateRight String String -> AccumulateRight String String -> Bool

-- 13.
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure' a) <> AccumulateBoth (Failure' a') = AccumulateBoth $ Failure' (a <> a')
  AccumulateBoth (Success' b) <> AccumulateBoth (Success' b') = AccumulateBoth $ Success' (b <> b')
  successLeft@(AccumulateBoth (Success' b)) <> _ = successLeft
  _ <> successRight@(AccumulateBoth (Success' b)) = successRight

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [ AccumulateBoth $ Failure' a
             , AccumulateBoth $ Success' b
             ]

type AccumulateBothValidation = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc) -- 1
  quickCheck (semigroupAssoc :: IdentityStringAssoc) -- 2
  quickCheck (semigroupAssoc :: TwoStringStringAssoc) -- 3
  quickCheck (semigroupAssoc :: ThreeStringOrderingStringAssoc) -- 4
  quickCheck (semigroupAssoc :: FourStringOrderingOrderingString) -- 5
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool) -- 6
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool) -- 7
  quickCheck (semigroupAssoc :: OrStringString) -- 8
  -- MISSING 9
  -- MISSING 10
  quickCheck (semigroupAssoc :: ValidationSS) -- 11
  quickCheck (semigroupAssoc :: AccumulateRightValidation) -- 12
  quickCheck (semigroupAssoc :: AccumulateBothValidation) -- 13
