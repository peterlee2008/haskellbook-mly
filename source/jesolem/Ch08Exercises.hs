
-- Ex 2
sumTo :: (Num a, Eq a) => a -> a
sumTo n
  | n == 0 = 0
  | otherwise = n + sumTo (n-1)


-- Ex 3
multiplyRecursive :: (Integral a) => a -> a -> a
multiplyRecursive x y
  | x == 0 = 0
  | otherwise = y + (multiplyRecursive (x-1) y)


mc91 :: (Num a, Ord a) => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = (mc91 . mc91) (n + 11)
