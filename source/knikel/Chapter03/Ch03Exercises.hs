-- Reading syntax
-- 1.
-- a) correct
-- b) incorrect, should've been called as (++) ... ...
-- c) correct
-- d) correct
-- e) incorrect, should be "hello" !! 4
-- f) correct
-- g) incorrect, should be take 4 "lovely"
-- h) correct

-- 2.
-- a -> d
-- b -> c
-- c -> e
-- d -> a
-- e -> b

-- Building functions
-- a) "Curry is awesome" ++ "!"
-- b) "Curry is awesome!" !! 4
-- c) drop 9 "Curry is awesome!"

-- 2.

module Ch03Exercises where

exclaim :: String -> String
exclaim xs = xs ++ "!"

index4 :: String -> Char
index4 xs = xs !! 4

drop9 :: String -> String
drop9 xs = drop 9 xs

-- 3.
thirdLetter :: String -> Char
thirdLetter xs = xs !! 3

-- 4.
letterIndex :: Int -> Char
letterIndex x= "Curry is awesome!" !! x

-- 5.
-- should work only with "Curry is awesome" -> "awesome is Curry"
rvrs :: String -> String
rvrs xs = awesome ++ is ++ curry where
  curry = take 5 xs
  is = take 4 (drop 5 xs)
  awesome = drop 9 xs
