eftBool :: Bool -> Bool -> [Bool]
eftBool f t
  | f == t = [f]
  | otherwise = f : eftBool (succ f) t

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd f t
  | f == t = [f]
  | otherwise = f : eftOrd (succ f) t

eftInt :: Int -> Int -> [Int]
eftInt f t
  | f == t = [f]
  | otherwise = f : eftInt (succ f) t

eftChar :: Char -> Char -> [Char]
eftChar f t
  | f == t = [f]
  | otherwise = f : eftChar (succ f) t
