module Ch10WithFolds where

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr ((||) . pred) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (== e)) False
-- VERBOSE: myElem x = foldr (\a b -> if a == x then True else b) False
myElem' e = any (== e)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\a b -> if pred a then a : b else b) []

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs
-- 'pluck' the first value off the list as the base case for the fold

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs
