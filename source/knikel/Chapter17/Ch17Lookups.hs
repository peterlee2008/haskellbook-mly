module Lookups where

import Data.List (elemIndex)
import Control.Applicative

-- add pure, <$>, or <*> to type-check

-- 1. ANSW: fmap
added :: Maybe Integer
added = fmap(+3) (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2. ANSW: <$>, <*>
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3. ANSW: <$>, <*>
x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'
-- max' gets partially applied to x', so its Just (max' x') <*> y'

-- 4. ANSW: Not sure, can't get this to work.
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
-- summed = sum $ (,) x'' y''
