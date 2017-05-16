-- 1. `length` type signature would be [a] -> Int. Close enough, the actual
--    type is Foldable t => t a -> Int

-- 2.
-- a) 5
-- b) 3
-- c) 2
-- d) 5

-- 3.
-- second example will break because `/` operates on Fractional type:
-- Fractional => a -> a -> a -> a

-- 4.
-- We should use `div`, which has a type definition of Integral a => a -> a -> a

--5.
-- Bool, we should expect True

-- 6.
-- Type is Bool, exepcted value is False

-- 7.
-- length allAwesome == 2 -- type is Bool, value True, won't reduce further
-- length [1, 'a', 3, 'b'] -- will throw an error, because lists can be of single type
--   here we're mixing Chars and Ints
-- length allAwesome + length awesome -- type Int, value 5
-- (8 == 8) && ('b' < 'a') - type Bool, value False, because 'b' is not < 'a' in ASCII tables
-- (8 == 8) && 9 -- will throw an error, because (8 == 8) is a Bool, 9 is Int, and `&&` has a type Bool -> Bool -> Bool

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.

myAbs :: Integer -> Integer
myAbs x =
  if x < 0
  then (-1) * x
  else x

-- 10.
f :: (a, b)-> (c, d) -> ((b, d), (a, c))
f x y = (,) ((,) (snd x) (snd y)) ((,) (fst x) (fst y))

-- CorrectingSyntax
-- 1. 'x', should be `x`, F should be f or any other lowercase name, since data constructors
--  start with capital letters
-- 2. should be id = (\x -> x), or id x = x
-- 3. should be e.g. f (x:xs) = x, since we pattern match on single argument which is a list
-- 4. should be f (a,b) = a


-- Match th function names to their types
-- 1. c
-- 2. b
-- 3. a
-- 4. d
