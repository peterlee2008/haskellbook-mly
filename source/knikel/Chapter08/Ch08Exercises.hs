-- Review of types
-- 1. d
-- 2. b
-- 3. d
-- 4. a

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- 1. partially applied cattyConny, with String -> String type sig
-- 2. parially applied flippy, with String -> String type sig
-- 3. "woops mrow2 mrowhaha"
-- 4. "woops mrowblue mrowhaha"
-- 5. "pink mrow haha mrow green mrowwoops mrowblue"


-- Recursion
{-- dividedBy 15 2 steps

dividedBy 15 2
-- otherwise
(15 - 2) 2 (0 + 1)
(13 - 2) 2 (1 + 1)
(11 - 2) 2 (2 + 1)
(9 - 2)  2 (3 + 1)
(7 - 2)  2 (4 + 1)
(5 - 2)  2 (5 + 1)
(3 - 2)  2 (6 + 1)
-- 1 < 2
1 < 2 = (7, 1)

--}

from1ToN :: (Eq a, Num a) => a -> a
from1ToN num = go num num where
  go n count
    | n == 0 = count
    | otherwise = go (n - 1) (count + n - 1)

-- 3 * 4 = 3 + 3 + 3 + 3
sumRec :: (Integral a) => a -> a -> a
sumRec num mult = go num mult 0 where
  go n m count
    | n == 0 = count
    | otherwise = go (n - 1) m (count + m)


-- Fixing dividedBy
data FixedDividedBy = Result Integer
  | DividedByZero
  deriving Show

fixedDividedBy :: Integer -> Integer -> FixedDividedBy
fixedDividedBy _ 0 = DividedByZero
fixedDividedBy num denom = go (abs num) denom 0 where
  go n d count
    | n < d = if num < 0 then Result (negate count) else Result count
    | otherwise = go (n - d) d (count + 1)

-- McCarthy91 function
mcCarthy91 :: (Integral a) => a -> a
mcCarthy91 x
  | x > 100 = x - 10
  | otherwise = 91

-- Numbers into words
-- See ./Ch08NumbersIntoWords.hs
