-- 6.
module Reverse where

-- should work only with "Curry is awesome" -> "awesome is Curry"
rvrs :: String -> String
rvrs xs = awesome ++ is ++ curry where
  curry = take 5 xs
  is = take 4 $ drop 5 xs
  awesome = drop 9 xs

main :: IO ()
main = print (rvrs "Curry is awesome")
