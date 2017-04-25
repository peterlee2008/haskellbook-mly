
-- Scans exercises --

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs
-- take 20 fibs
-- takeWhile (<100) fibs

fact :: [Integer]
fact = scanl (*) 1 [1..]
-- take 10 fact


-- Ex 1
stops  = "pbtdkg"
vowels = "aeiou"
combinations = [[x] ++ [y] ++ [z] | x <- stops, y <- vowels, z <- stops]
justp = [[x] ++ [y] ++ [z] | x <- stops, y <- vowels, z <- stops, x == 'p']

-- Ex 3 (gives average word length)
seekritFunc :: String -> Double
seekritFunc x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))


-- Writing with folds --

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem:: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (== a)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap::(a->b)->[a]->[b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\ x res -> if f x then x:res else res) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\ x y -> if f x y == GT then x else y)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\ x y -> if f x y == LT then x else y)
