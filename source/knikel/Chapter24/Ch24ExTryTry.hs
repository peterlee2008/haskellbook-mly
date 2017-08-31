{-# LANGUAGE QuasiQuotes #-}
module Ch24ExTryTry where

import Control.Applicative ((<|>))
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

type FractionOrDec = Either Rational Double

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "The denominator cannot be 0"
    _ -> return $ (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  beforePoint <- decimal
  char '.'
  afterPoint <- decimal
  return $ (+) (realToFrac beforePoint)
    (mantissa afterPoint) where
    mantissa x = (realToFrac x) / (realToFrac (10 ^ (digitCount x)))
    digitCount :: Integer -> Int
    digitCount = go 1 . abs
      where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

parseFracOrDec :: Parser FractionOrDec
parseFracOrDec = do
  skipMany (oneOf "\n")
  v <- try (Left <$> parseFraction) <|> try (Right <$> parseDecimal)
  skipMany (oneOf "\n")
  return v

eitherOr :: String
eitherOr = [r|
123.0
1/23
5.33423414315
|]

main = do print $ parseString (many parseFracOrDec) mempty eitherOr
