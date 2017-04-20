module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar shift x
  | shift == 0 = x
  | (ordx + shift) < orda = chr ((ordx + 26 + shift) `mod` orda + orda)
  | otherwise = chr ((ordx + shift) `mod` orda + orda)
  where
    orda = ord 'a'
    ordx = ord x


caesar :: Int -> String -> String
caesar shift code = map (shiftChar shift) code

unCaesar :: Int -> String -> String
unCaesar shift code = map (shiftChar $ negate shift) code
