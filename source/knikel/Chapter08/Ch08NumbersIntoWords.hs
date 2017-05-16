module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "" -- yeah, I know...

digits :: Int -> [Int]
digits num = go num 10 [] where
  go :: Int -> Int -> [Int] -> [Int]
  go n d acc
    | div n d == 0 = (mod n d) : acc
    | otherwise = go (div n d) d $ (mod n d) : acc

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
