module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Data.Bool (bool)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

newtype GuessCount = GuessCount (Int, Int) deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] GuessCount

instance Show Puzzle where
  show (Puzzle _ discovered guessed _) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) [] (GuessCount (0, 5))

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle xs _ _ _) c = elem c xs

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ xs _) c = elem c xs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s (GuessCount (count, bound))) c =
  Puzzle
  word
  newFilledInSoFar
  (c : s)
  (GuessCount $ (bool (count + 1) count (elem (Just c) newFilledInSoFar), bound))
  -- [1] - handle incrementation of invalid guess counts
  -- this could be cleaned up though
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed (GuessCount (count, bound))) =
  if count == bound then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ (GuessCount count)) =
  if all isJust filledInSoFar then
    do putStrLn $ "You win! Only " ++ show count ++ "wrong guesses!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle@(Puzzle _ _ _ (GuessCount (count, bound))) = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr $ "[" ++ show (bound - count) ++ " left] - " ++ "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                    \ be a single character"

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess
       ) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
               \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
