module Ch11AsPatterns where

import Data.Char

-- why not isSub (x:xs) ys = elem x ys && isSub xs ys
-- the values don't have to be contiguous
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _  = True
isSubsequenceOf _ []  = False
isSubsequenceOf xsall@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xsall ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map capitalizeW $ words sentence where
  capitalizeW xsall@(x:xs) = (xsall, toUpper x  : xs )
