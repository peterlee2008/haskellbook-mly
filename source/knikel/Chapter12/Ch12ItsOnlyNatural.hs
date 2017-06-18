module Ch12ItsOnlyNatural where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ validIntToNat x where
      validIntToNat 0 = Zero
      validIntToNat i = (Succ (validIntToNat $ i - 1))
