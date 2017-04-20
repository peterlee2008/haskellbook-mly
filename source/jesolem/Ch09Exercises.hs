
-- Ex EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool from to
  | from == to = [from]
  | otherwise = from : eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
  | from == to = [from]
  | from > to = []
  | otherwise = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from == to = [from]
  | from > to = []
  | otherwise = from : eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from == to = [from]
  | from > to = []
  | otherwise = from : eftChar (succ from) to


-- Ex Fearful symmetry
myWords :: String -> [String]
myWords x
  | x == [] = []
  | otherwise = takeWhile (/= ' ') x : [] ++ myWords (dropWhile (== ' ') (dropWhile (/= ' ') x))


-- Ex 3: rewrite with parameter
myWordsParam :: Char -> String -> [String]
myWordsParam c x
  | x == [] = []
  | otherwise = takeWhile (/= c) x : [] ++ myWordsParam c (dropWhile (== c) (dropWhile (/= c) x))


-- Ex Square Cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
-- print [(x, y) | x <- mySqr, y <- myCube]
-- print [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
-- length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Ex Filtering
-- filter (\x -> (rem x 3) == 0) [1..30]

myFilter :: String -> [String]
myFilter x
  | x == [] = []
  | otherwise = filter (/= "a") (words x)
