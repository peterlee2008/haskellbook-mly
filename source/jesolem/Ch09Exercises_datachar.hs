import Data.Char

pickUpper :: String -> String
pickUpper [] = []
pickUpper (x:xs)
  | isUpper x = x : pickUpper xs
  | otherwise = pickUpper xs

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

capitalizeOnlyFirst :: String -> Char
capitalizeOnlyFirst = (toUpper . head)
