module Ch10DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9900
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs where
  f :: DatabaseItem -> [UTCTime] -> [UTCTime]
  f (DbDate x) acc = x : acc
  f _ x = x


-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs where
  f :: DatabaseItem -> [Integer] -> [Integer]
  f (DbNumber x) acc = x : acc
  f _ x = x


-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.

sumDb :: [DatabaseItem] -> Integer
sumDb = foldl (+) 0 . filterDbNumber


-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral $ div s len where
  list = filterDbNumber xs
  s = foldl (+) 0 list
  len = toInteger $ length list
