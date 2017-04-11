
-- Ex 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b


-- Ex 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = (a == c) && (b == d)


-- Ex 3
data StringOrInt = TisAnInt Int | TisAString String deriving Show

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False


-- Ex 4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = (a == c) && (b == d)


-- Ex 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) =>  Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = (a == c) && (b == d)


-- Ex 6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False


-- Ex 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False
