module Ciphers where

import Data.Char

salt :: Int -> Char -> Char
salt _ ' ' = ' '
salt shift x = atoz !! mod ((ord x - orda) + shift) span where
  orda = ord 'a'
  span = length atoz
  atoz = ['a'..'z']

caesar xs shift = map (salt shift) xs
unCaesar xs shift = map (salt $ negate shift) xs


-- it's safe to use the !! function, despite the fact
-- it's not a total function, because we never go further than the length
-- of chars
