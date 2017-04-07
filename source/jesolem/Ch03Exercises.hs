module Reverse where

-- Ex 3
thirdLetter :: String -> Char
thirdLetter xs = xs !! 3

-- Ex 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- Ex 5
rvrs :: String -> String
rvrs x = a ++ b ++ c
  where
    a = take 7 (drop 9 x)
    b = take 4 (drop 5 x)
    c = take 5 x


main :: IO ()
-- main = print $ thirdLetter "Curry is awesome!"
-- main = print $ letterIndex 4
main = print $ rvrs "Curry is awesome!"
