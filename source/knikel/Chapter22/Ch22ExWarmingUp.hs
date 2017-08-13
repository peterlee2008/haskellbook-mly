module Ch22ExWarmingUp where

import Control.Monad (join)
import Control.Applicative (liftA2)
import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

--

composed :: [Char] -> [Char]
composed xs = cap . rev $ xs

fmapped :: [Char] -> [Char]
fmapped xs = fmap cap rev $ xs

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  return (a, b)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= \a -> rev >>= \b -> return (a, b)
