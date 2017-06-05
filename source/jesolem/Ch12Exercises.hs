
-- Ex 1
notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise = Just x

replaceThe :: String -> String
replaceThe = unwords . map (\x -> if x == "the" then "a" else x) . words


-- Ex 2
vowels = "aeiuo"

nbrThe :: [String] -> Integer
nbrThe (x:y:xs)
  | (x == "the") && (elem (head y) vowels) = 1 + nbrThe xs
  | otherwise = nbrThe xs
nbrThe _ = 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = nbrThe $ words x


-- Ex 3
countVowels :: String -> Integer
countVowels s = toInteger $ length [x | x <- s, elem x vowels]


-- Validate the word
countConsonants :: String -> Integer
countConsonants s = toInteger $ length [x | x <- s, notElem x vowels]

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w
  | countVowels w > countConsonants w = Nothing
  | otherwise = Just (Word' w)
