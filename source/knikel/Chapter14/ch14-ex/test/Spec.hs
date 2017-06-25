-- Validating numbers into words

import Test.Hspec
import Test.QuickCheck

import WordNumber (digitToWord, digits, wordNumber)
import Data.List (sort)
import Data.Char (toUpper)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one given 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

-- Using QuickCheck
  describe "Using QuickCheck" $ do
    it "1. half" $ do
      property $ \x -> prop_halfIdentity (x :: Double)
    it "2. import Data.List (sort)" $ do
      property $ \xs -> prop_listOrdered (xs :: [Char])
    it "3. addition associative" $ do
      property $ \x -> prop_plusAssociative (x :: (Integer, Integer, Integer))
    it "3. addition commutative" $ do
      property $ \x -> prop_plusCommutative (x :: (Integer, Integer))
    it "4. multiplication associative" $ do
      property $ \x -> prop_multAssociative (x :: (Integer, Integer, Integer))
    it "4. multiplication commutative" $ do
      property $ \x -> prop_multCommutative (x :: (Integer, Integer))
    it "5. quot/rem law" $ do
      property $ \x -> prop_quotRemLaw (x :: (Integer, Integer))
    it "5. div/mod law" $ do
      property $ \x -> prop_divModLaw (x :: (Integer, Integer))
    -- FAILING
    xit "6. ^ is not commutative" $ do
      property $ \x -> prop_powCommutative (x :: (Integer, Integer))
    -- FAILING
    xit "6. ^ is not associative" $ do
      property $ \x -> prop_powAssociative (x :: (Integer, Integer, Integer))
    it "7. reverse . reverse is id" $ do
      property $ \xs -> prop_reverse (xs :: [String])

    it "8. $" $ do
      property $ \x -> prop_dollar (x :: (Integer, Integer))

-- 1.
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just x, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

-- 3.

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_plusAssociative :: (Eq a, Num a) => (a, a, a) -> Bool
prop_plusAssociative (x, y, z) = plusAssociative x y z

prop_plusCommutative :: (Eq a, Num a) => (a, a) -> Bool
prop_plusCommutative (x, y) = plusCommutative x y

-- 4.
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_multAssociative :: (Eq a, Num a) => (a, a, a) -> Bool
prop_multAssociative (x, y, z) = multAssociative x y z

prop_multCommutative :: (Eq a, Num a) => (a, a) -> Bool
prop_multCommutative (x, y) = multCommutative x y

-- 5.
quotRemLaw :: (Eq a, Integral a) => a -> a -> Bool
quotRemLaw x y = y == 0 || (quot x y) * y + (rem x y) == x

prop_quotRemLaw :: (Eq a, Integral a) => (a, a) -> Bool
prop_quotRemLaw (x, y) = quotRemLaw x y

divModLaw :: (Eq a, Integral a) => a -> a -> Bool
divModLaw x y = y == 0 || (div x y) * y + (mod x y) == x

prop_divModLaw :: (Eq a, Integral a) => (a, a) -> Bool
prop_divModLaw (x, y) = divModLaw x y


-- 6.
powAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
powAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

prop_powAssociative :: (Eq a, Integral a) => (a, a, a) -> Bool
prop_powAssociative (x, y, z) = powAssociative x y z

powNotCommutative :: (Eq a, Integral a) => a -> a -> Bool
powNotCommutative x y = x ^ y == y ^ x

prop_powCommutative :: (Eq a, Integral a) => (a, a) -> Bool
prop_powCommutative (x, y) = powNotCommutative x y

-- 7.

rev :: (Eq a) => [a] -> Bool
rev xs = ((reverse . reverse) xs) == id xs

prop_reverse :: (Eq a) => [a] -> Bool
prop_reverse = rev

-- 8.
prop_dollar :: (Num a, Eq a) => (a, a) -> Bool
prop_dollar (x, y) = ((+) x $ (+) x y) == (+) x ((+) x y)
-- I couldn't figure out how to get QuickCheck to generate a function with (Num a) => a -> a -> a type

-- 9. TODO

-- 10.
-- No, since length of take [] will always be zero, despite the n

-- 11. TODO

-- Failure
-- It fails because floating point division is imprecise

-- Idempotence

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord xs = xs

-- TODO: f x = (capitalizeWord x == twice capitalizeWord x) == fourTimes capitalizeWord x
-- TODO: f x = (sort x == twice sort x) == (fourTimes sort x)

-- Make a Gen random generator for the datatype

data Fool =
    Fulse
  | Frue deriving (Eq, Show)

-- 1.
foolGen :: Gen Fool
foolGen = do
  oneof [ return $ Fulse
        , return $ Frue
        ]

-- 2.
foolGen13 :: Gen Fool
foolGen13 = do
  frequency [ (1, return $ Fulse)
            , (3, return $ Frue)
            ]

-- Hangman tests TODO

-- Validating ciphers TODO
