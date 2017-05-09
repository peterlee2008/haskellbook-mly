module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar shift x
  | shift == 0 = x
  | x == ' ' = x
  | (ordx + shift) < orda = chr ((ordx + 26 + shift) `mod` orda + orda)
  | otherwise = chr ((ordx + shift) `mod` orda + orda)
  where
    orda = ord 'a'
    ordx = ord x


caesar :: Int -> String -> String
caesar shift code = map (shiftChar shift) code

unCaesar :: Int -> String -> String
unCaesar shift code = map (shiftChar $ negate shift) code

-- Above is from the Ch9 exercise
-- This is the key phrase
type Key = String

vignere :: Key -> String -> String
vignere key mess = shiftKey (zip mess (cycle key))
  where
    shiftKey = map (\(c,s) -> shiftChar ((ord s) - (ord 'a')) c )

unVignere :: Key -> String -> String
unVignere key code = shiftKeyInverse (zip code (cycle key))
  where
    shiftKeyInverse = map (\(c,s) -> shiftChar (negate ((ord s) - (ord 'a'))) c )

-- Use lowercase instead
message = "meet at dawn"
keyphrase = "ally"

main :: IO ()
main = do
  print $ unVignere keyphrase (vignere keyphrase message)
