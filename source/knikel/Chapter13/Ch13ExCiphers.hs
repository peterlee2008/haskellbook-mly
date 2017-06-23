module Ch13ExCiphers where

import Data.Bool (bool)
import System.Exit (exitSuccess)

-- needs a ln -s ../Chapter11/Ch11Ciphers.hs .
-- since we don't like copying files, nor setting up
-- nested stack projects
import Ch11Ciphers

runCipher :: Char -> String -> String -> String
runCipher 'v' word salt = vigenere word salt
runCipher 'c' word salt = caesar word (read salt :: Int)

validateCipherChoice :: Char -> IO Bool
validateCipherChoice 'c' = return True
validateCipherChoice 'v' = return True
validateCipherChoice _ = exitSuccess

main :: IO ()
main = do
  putStrLn $ "Type v for vigenere, c for caesar"
  cipher <- getLine
  validateCipherChoice $ head cipher
  putStrLn $ "Type the word you want to encode"
  word <- getLine
  putStrLn $ "You've chosen [" ++ cipher ++ "] use a " ++ (bool "String" "Int" ("c" == cipher))
  salt <- getLine
  putStrLn $ runCipher (head cipher) word salt
  return exitSuccess ()
