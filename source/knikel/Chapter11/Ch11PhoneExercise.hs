module Ch11PhoneExercise where

import Data.List
import Data.Char

{--

----------------------------
| 1      | 2 ABC  | 3 DEF  |
----------------------------
| 4 GHI  | 5 JKL  | 6 MNO  |
----------------------------
| 7 PQRS | 8 TUV  | 9 WXYZ |
----------------------------
| * ^    | 0 + _  | # .,   |
----------------------------

--}

-- To represent Buttons identifiers
data ButtonId =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Star
  | Zero
  | Hash
  deriving (Eq, Show)

-- To represent Buttons as ButtonId with content of keys
data Button = Button ButtonId [Char] deriving (Eq, Show)

-- To represent if the Star button has been pressed
data Modifier = None | Upper

data DaPhone = DaPhone [(ButtonId, [Char])] deriving (Eq, Show)

-- Valid buttons: 1234567890*#
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

phone :: DaPhone
phone = DaPhone [
  one, two, three,
  four, five, six,
  seven, eight, nine,
  star, zero, hash
  ] where
  one = (One, ['1'])
  two = (Two, ['a', 'b', 'c', '2'])
  three = (Three, ['d', 'e', 'f', '3'])
  four = (Four, ['g', 'h', 'i', '4'])
  five = (Five, ['j', 'k', 'l', '5'])
  six = (Six, ['m','n', 'o', '6'])
  seven = (Seven, ['p', 'q', 'r', 's', '7'])
  eight = (Eight, ['t', 'u', 'v', '8'])
  nine  = (Nine, ['w', 'x', 'y', 'z', '9'])
  star = (Star, [])
  zero = (Zero, [' ', '0'])
  hash = (Hash, ['.', ','])

reverseTaps :: DaPhone -> Char -> [(ButtonId, Presses)]
reverseTaps p@(DaPhone phonePad) c
  | isUpper c = [(Star, 1)] ++ reverseTaps (DaPhone phonePad) (toLower c)
  | otherwise = [(fst keyContent, indexToInt index)] where
      index = elemIndex c $ snd keyContent
      keyContent = head $ filter (\button -> elem c $ snd button) phonePad
      indexToInt (Just num) = num + 1
      indexToInt Nothing = 0

cellPhonesDead :: DaPhone -> String -> [(ButtonId, Presses)]
cellPhonesDead p xs = concat $ map (\x -> reverseTaps p x) xs

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making usre rofl ur turn"
  ]

fingerTaps :: [(ButtonId, Presses)] -> Presses
fingerTaps xs = foldr (\a b -> (snd a) + b) 0 xs

mostPopularLetter :: String -> Char
mostPopularLetter xs = fst $ head sorted where
  sorted :: [(Char, Int)]
  sorted = sortBy (\a b -> compare (snd b) (snd a)) $ list
  list :: [(Char, Int)]
  list = foldr (\a b -> (a, length $ filter (==a) xs) : b) [] xs

coolestLtr :: [String] -> Char
coolestLtr xs = mostPopularLetter $ concat xs

coolestWord :: [String] -> String
coolestWord xs = fst $ head $ sorted where
  sorted = sortBy (\a b -> compare (snd b) (snd a)) list
  ws = words $ intercalate " " xs
  list = nub $ foldr (\a b -> (a, length $ filter (==a) ws) : b) [] ws

main :: IO ()
main = do
  putStrLn $ show $ map (cellPhonesDead phone) convo
