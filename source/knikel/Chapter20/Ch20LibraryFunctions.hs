module Ch20LibraryFunctions where

import Data.Foldable (foldMap, foldr)
import Data.Monoid

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs

testSum = do
  print $ "[1,2,3] = " ++ show (sum' [1,2,3])
  print $ "Just 5 = " ++ show (sum' (Just 5))
  print $ "Just [1,2,3] = " ++ (show $ (fmap sum' (Just [1,2,3 :: Int])))

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap Product xs

testProduct = do
  print $ "[1,2,3] = " ++ show (product' [1,2,3])
  print $ "Just 5 = " ++ show (product' (Just 5))
  print $ "Just [1,2,3] = " ++ (show $ (fmap product' (Just [1,2,3 :: Int])))

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e xs = getAny $ foldMap (\el -> Any $ e == el) xs

testElem = do
  print $ "[1,2,3] -- elem' 1 = " ++ show (elem' 1 [1,2,3])
  print $ "Just 5 -- elem' 1 = " ++ show (elem' 1 (Just 5))
  print $ "Just 5 -- elem' 5 = " ++ show (elem' 5 (Just 5))
  print $ "Just [1,2,3,4,5] -- elem' 5 = " ++ show (fmap (elem' 5) (Just [1..5 :: Int]))

-- 4.
-- This was a fun one.
-- 1. We need to define a new type to wrap our values
data Minimum a = Minimum { getMinimum :: Maybe a } deriving (Eq, Show)

-- 2. We need to define a Monoid instance of our new type,
-- in this case we're going to reduce the monoidal values by finding the minimum of two
-- values passed to `mappend`
instance Ord a => Monoid (Minimum a) where
  mempty = Minimum Nothing
  (Minimum Nothing) `mappend` x = x
  x `mappend` (Minimum Nothing) = x
  (Minimum a) `mappend` (Minimum a') = Minimum $ min a a' -- This is where the magic happens

-- We have to componse the Minimum with Just because the return
-- type in this exercise is Maybe a
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getMinimum $ foldMap (Minimum . Just) xs

-- 5.
-- Similar story as Minimum, but this time we'll be reducing the monoidal values by finiding
-- the maximum of two values passed to `mappend`

data Maximum a = Maximum { getMaximum :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Maximum a) where
  mempty = Maximum Nothing
  (Maximum Nothing) `mappend` x = x
  x `mappend` (Maximum Nothing) = x
  (Maximum a) `mappend` (Maximum a') = Maximum $ max a a'

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = getMaximum $ foldMap (Maximum . Just) xs

-- 6.
-- If we fold at least one element, this means the Foldable instance
-- is not empty. We can ignore any parameters in the folding function and
-- just return False. Otherwise we return True, which is our 'zero' value.
null :: (Foldable t) => t a -> Bool
null xs = foldr (\_ _ -> False) True xs

-- 7.
length' :: (Foldable t) => t a -> Int
length' xs = foldr (\_ x -> x + 1) 0 xs

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' xs = foldr (:) [] xs

-- 9.
-- This works because foldMap _delegates_ to `mappend` the reduction of the structure
-- through the Monoid m constraint. `id` just returns the value, <> is where stuff happens.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap id xs

-- 10.
-- fold - mappend
-- map - f
-- fold(map) is like mappend(f)
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (mappend . f) mempty xs
