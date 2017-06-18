module Ch12ValidateTheWord where

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs = validate $ ratio xs where
  validate (vow, con)
    | vow > con = Nothing
    | otherwise = Just (Word' xs)
  ratio xs' = (countVowels xs, (length xs) - (countVowels xs))
  countVowels xs' = length $ (filter (flip elem vowels) xs')
