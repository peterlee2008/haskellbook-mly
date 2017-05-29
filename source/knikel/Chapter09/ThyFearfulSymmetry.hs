{--
breakWord :: String -> [String]
breakWord = flip splitAtChar ' '
--}

module PoemLines where

splitAtChar :: String -> Char -> [String]
splitAtChar [] _ = []
splitAtChar xs c = [takeWhile (/= c) xs] ++ splitAtChar (dropWhile (== c) $ dropWhile (/= c) xs) c

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = flip splitAtChar '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
