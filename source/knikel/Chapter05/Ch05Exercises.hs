-- Multiple choice
-- 1. c
-- 2. a
-- 3. b
-- 4. c, d

-- Determine the type

-- 1.
-- a. Num a => a -- 54
-- b. (Num a) => (a, [Char]) -- (0, "doge")
-- c. (Integer, [Char]) -- (0, "doge")
-- d. Bool -- False
-- e. Int -- 5
-- f. Bool -- False

-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a
-- 5. [Char]

-- Does it compile?
-- 1. bigNum doesn't accept any arguments and it's meant to be used as bigNum $ 10
--    therefore `bigNum = (^) 5` is correct solution.
-- 2. No problems
-- 3. c = b 10, should be c = a 10, since a = (+)
-- 4. we need c declared somewhere

-- Type variable or specific type constructor?
-- 2. f :: zed -> Zed -> Blah
--         [0]    [1]    [2]
-- 0 - fully polymorphic
-- 1 - concrete
-- 2 - concrete

-- 3. f :: Enum b => a -> b -> C
--                  [0]  [1]  [2]
-- 0 - fully polymorphic
-- 1 - constrained polymorphic
-- 2 - concrete

-- 4. f :: f -> g -> C
--        [0]  [1]  [2]
-- 0 - fully polymorphic
-- 1 - fully polymorphic
-- 2 - concrete

-- Write a type signature
-- 1. [a] -> a
-- 2. (Ord a, Ord b) => a -> b -> Bool
-- 3. (a, b) -> b

-- Given a type write a function
{--
myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ (xToY x)))


i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b

-- c and c'' are _the same_ given alpha equivalence

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r as = take 0 as

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a
--}

-- Fix it
{--
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
  where x = "Singing"
        y = "Somewhere"
--}

{--
module Arith3Broken where

main :: IO ()
main = do
  print $ 1 + 2
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1
--}

-- Type-Kwon-Do

-- 1.

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x

-- 2.
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xfrom :: (X, Y) -> (Z, Z)
xfrom (x, y) = (xz x, yz y)

-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = fst $ yToWZ $ xToY x
