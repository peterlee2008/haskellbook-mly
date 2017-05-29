module Standard where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = any (\x -> a == x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- An original maximumBy throws on an empty list, therefore we assume
-- a non empty list. The most important piece is the go function, at
-- the time we call it we have unfolded two values off the list (since we
-- handle the case for single item list on the first pattern match). While
-- we have two values, it's possible to compare them and get ordering.
-- The go function is an accumulator style function that carries over
-- some data.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = go f x xs where
  go :: (a -> a -> Ordering) -> a -> [a] -> a
  go f x1 (x2:xs)
    | f x1 x2 == GT = go f x1 xs
    | otherwise = go f x2 xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = myMaximumBy (flip f) xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMinimumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMaximumBy compare
