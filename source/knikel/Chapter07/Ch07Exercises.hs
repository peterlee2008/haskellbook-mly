-- Multiple choice
-- 1. d
-- 2. b
-- 3. d
-- 4. b
-- 5. a

-- Let's write code
-- 1.

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

-- a.
tensDigit' :: Integral a => a -> a
tensDigit' x = f snd . f fst $ x where
  f fn = fn . (flip divMod) 10

-- b. yes

-- c.
hunsD :: Integral a => a -> a
hunsD x = f snd 10 . f snd 100 . f fst 100 $ x where
  f fn y = fn . (flip divMod) y

-- 2.

foldBool :: a -> a -> Bool -> a
foldBool a b cond = case cond of
  True  -> a
  False -> b

foldBool' :: a -> a -> Bool -> a
foldBool' a b cond
  | cond = a
  | otherwise = b

-- 3.

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


-- 4.

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

-- print (roundTrip' 4 :: Integer) -- WORKS!
