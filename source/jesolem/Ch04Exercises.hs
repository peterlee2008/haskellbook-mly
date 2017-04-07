
-- Ex 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == reverse x)

-- Ex 9
myAbs :: Integer -> Integer
myAbs x = if (x<0) then (-x) else x

-- Ex 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
