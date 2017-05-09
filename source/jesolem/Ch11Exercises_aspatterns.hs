import Data.Char
import Data.List.Split

-- Ex 1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xx@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xx ys


-- Ex 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x, capitalize x)) . words
  where
    capitalize (x:xs) = toUpper x : xs


-- Language exercises

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph p =  foldr (\x y -> x ++ ". " ++ y) [] (map capitalizeWord (splitOn ". " p))
