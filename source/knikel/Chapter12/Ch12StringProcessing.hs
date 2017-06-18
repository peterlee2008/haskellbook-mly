module Ch12StringProcessing where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe xs = Just xs

replaceThe :: String -> String
replaceThe xs = unwords $ replaceThe' $ map notThe $ words xs where
  replaceThe' :: [Maybe String] -> [String]
  replaceThe' [] = []
  replaceThe' (Nothing:xs) = "a" : replaceThe' xs
  replaceThe' (Just x:xs) = x : replaceThe' xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = countStuff (words xs) 0 where
  vowels = "aeiouy"
  countStuff :: [String] -> Integer -> Integer
  countStuff [] b = b
  countStuff ("the":x:xs) b =
    if elem (head x) vowels
    then countStuff xs (b + 1)
    else countStuff xs b
  countStuff (_:xs) b = countStuff xs b

countVowels :: String -> Integer
countVowels xs = foldr addIfVowel 0 xs where
  vowels = "aeiouy"
  isVowel x = elem x vowels
  addIfVowel a b = if isVowel a then b + 1 else b
