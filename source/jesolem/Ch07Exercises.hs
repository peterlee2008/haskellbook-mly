
-- Ex 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))


-- Ex 1: functionC x y = if (x > y) then x else y
functionC x y = case (x > y) of
  True -> x
  False -> y

-- EX 2: ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n = case mod n 2 == 0 of
  True -> n + 2
  False -> n

-- Ex 3
nums :: Integer -> Integer
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0


-- Function composition --

-- Ex 1
tensDigit :: Integral a => a -> a
tensDigit x = snd (divMod (fst (divMod x 10)) 10)

hunDigit :: Integral a => a -> a
hunDigit x = snd (divMod (fst (divMod x 100)) 10)


-- Ex 2
foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
  | b = x
  | otherwise = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
  case b of
    True -> x
    otherwise -> y


-- Ex 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
