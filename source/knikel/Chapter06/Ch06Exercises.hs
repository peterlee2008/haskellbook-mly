-- Multiple choice
-- 1. c
-- 2. a, b
-- 3. a
-- 4. c
-- 5. a

-- Does it type check?
-- 1. no, no Show instance for Person type

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- 2. no, no Eq instance for Mood type

data Mood = Blah | Woot deriving Show

instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _       = False

settleDown x = if x == Woot
               then Blah
               else x

-- 3.
-- a) Blah or Woot
-- b) The compiler will throw an error, 9 is not a valid Mood type
-- c) error, Mood doesn't have a typeclass instance of Ord

-- 4. No. s1 is partially applied function (Object), functions don't have instances of Show
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, whate can we do?
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- 1. phew = Papu "chases" True
-- should be: Papu (Rocks "chases") (Yeah True)

-- 2.
truth = Papu (Rocks "chomskdoz") (Yeah True)

-- 3.
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4. No, Papu, Rocks, Yeah don't have Ord typeclass instances
-- FIX: add deriving Ord to Rocks, Yeah and Papu
--
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

-- Type-Kwon-Do

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = (+) (aToB a) (aToB a)
