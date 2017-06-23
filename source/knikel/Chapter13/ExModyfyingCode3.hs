import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

normalizeLine xs = filter isAtoZ $ map toLower xs where
  isAtoZ x = elem x atoz
  atoz = ['a'..'z']

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (normalizeLine line1 == reverse (normalizeLine line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nopez" where
  return exitSuccess ()
