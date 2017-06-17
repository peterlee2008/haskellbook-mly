module Ch11LanguageExercises where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord _ = []

capitalizeParagraph :: String -> String
capitalizeParagraph para = go $ capitalizeWord para where
  go xsall@('.':' ':x:xs) = ". " ++ capitalizeWord [x] ++ go xs
  go (x:xs) = x : go xs
  go _ = []
