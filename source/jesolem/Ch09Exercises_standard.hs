-- Ex 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- Ex 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- Ex 3
myElem:: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = x == a || myElem a xs

-- Ex 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Ex 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- Ex 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- Ex 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Ex 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x xs
  where
    go _ myMax [] = myMax
    go f myMax (x:xs)
      | f myMax x == LT = go f x xs
      | otherwise = go f myMax xs

-- Ex 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = myMaximumBy (flip f) xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
