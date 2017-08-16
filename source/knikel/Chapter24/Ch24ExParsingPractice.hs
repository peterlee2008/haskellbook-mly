module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' >> eof >> stop

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' >> eof >> stop

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

oneString :: Parser String
oneString = string "1"

oneTwoString :: Parser String
oneTwoString = string "12" >> stop

oneTwoThreeString :: Parser String
oneTwoThreeString = string "123"

allThree :: Parser String
allThree = choice [ oneTwoThreeString
                  , oneTwoString
                  , oneString
                  ]

-- 1. easy
-- 2. not sure if allThree is up to standard
-- 3. TODO: I'm lost

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneString:"
  testParse' oneString
  pNL "oneTwoString:"
  testParse' oneTwoString
  pNL "oneTwoThreeString:"
  testParse' oneTwoThreeString
  pNL "allThree:"
  testParse' allThree
