import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbNumber 9002,
    DbString "Hello, world!",
    DbDate (UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123))
  ]

-- Ex 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr appendTime []
  where
    appendTime (DbDate time) x = time : x
    appendTime _ x = x

-- Ex 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr appendInteger []
  where
    appendInteger (DbNumber num) x = num : x
    appendInteger _ x = x

-- Ex 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- Ex 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- Ex 5
avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length (filterDbNumber xs))
