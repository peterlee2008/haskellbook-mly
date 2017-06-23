module Ch11Ciphers where

import Data.Char
import Data.List

-- START: taken from Ch09
salt :: Int -> Char -> Char
salt _ ' ' = ' '
salt shift x = atoz !! mod ((ord x - orda) + shift) span where
  orda = ord 'a'
  span = length atoz
  atoz = ['a'..'z']

caesar :: String -> Int -> String
caesar xs shift = map (salt shift) xs

unCaesar :: String -> Int -> String
unCaesar xs shift = map (salt $ negate shift) xs

splitAtChar :: String -> Char -> [String]
splitAtChar [] _ = []
splitAtChar xs c = [takeWhile (/= c) xs] ++ splitAtChar (dropWhile (== c) $ dropWhile (/= c) xs) c
-- END

mapSalt :: String -> String -> String
mapSalt xs word = intercalate " " $ go splitted cycled 0 where
  splitted = splitAtChar xs ' '
  cycled = take (length $ concat splitted) $ cycle word

  go :: [String] -> String -> Int -> [String]
  go [] _ _ = []
  go (s:ss) cyc seenChars = take sLen nextCyc : go ss nextCyc sLen where
    sLen = length s
    nextCyc = drop seenChars cyc

vigenere :: String -> String -> String
vigenere xs w = concat $ zipWith (\a b -> caesar [a] b) xs $ map ord $ mapSalt xs w
unVigenere :: String -> String -> String
unVigenere xs w = concat $ zipWith (\a b -> unCaesar [a] b) xs $ map ord $ mapSalt xs w

testMapping :: IO ()
testMapping =
  if mapSalt xs word == mapped
  then putStrLn "OK: mapSalt xs word"
  else putStrLn $ (mapSalt xs word) ++ ": /= :" ++ mapped where
    xs = "meet at dawn"
    word = "ally"
    mapped = "ally al lyal"

main :: IO ()
main = do
  testMapping
  putStrLn ""
  putStrLn $ "vigenere: \"" ++ xs ++ "\" \"" ++ w ++ "\""
  putStrLn $ "TO: " ++ vigenere xs w
  putStrLn $ "unVigenere: \"" ++ vigenere xs w ++ "\" \"" ++ w ++ "\""
  putStrLn $ "TO: " ++ unVigenere (vigenere xs w) w where
    xs = "meet me at some point somewhere"
    w = "wat"
