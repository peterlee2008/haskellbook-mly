module Ch24ExSemver where

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving Show

instance Eq NumberOrString where
  (==) (NOSI i) (NOSI i') = i == i'
  (==) (NOSS s) (NOSS s') = s == s'
  (==) _ _ = False

instance Ord NumberOrString where
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI _) _         = LT
  compare _        (NOSI _)  = GT

instance Eq SemVer where
  (==) (SemVer ma mi pa re me) (SemVer ma' mi' pa' re' me') =
    ma == ma' && mi == mi' && pa == pa' && re == re' && me == me'

instance Ord SemVer where
  compare (SemVer ma mi p re _) (SemVer ma' mi' p' re' _) =
    let comparison = (compare ma ma') <> (compare mi mi') <> (compare p p') in
      case comparison of
        EQ -> compare re re'
        _  -> comparison

parseNos :: Parser NumberOrString
parseNos = do
  (NOSI <$> decimal)
  <|>
  (NOSS <$> some letter)

parseDotNos :: Parser NumberOrString
parseDotNos = do
  nos <- parseNos
  _ <- skipMany (oneOf ".")
  return nos

parseDotsAfter :: Char -> Parser [NumberOrString]
parseDotsAfter c = do
  (char c >> many parseDotNos)
  <|>
  return []

parseRelease :: Parser [NumberOrString]
parseRelease = parseDotsAfter '-'

parseMetadata :: Parser [NumberOrString]
parseMetadata = parseDotsAfter '+'

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- skipDot
  minor <- decimal
  _ <- skipDot
  patch <- decimal
  rel <- try parseRelease
  metadata <- try parseMetadata
  return $  SemVer major minor patch rel metadata
  where skipDot = oneOf "."

main :: IO ()
main = do
  print $ parseString (token parseSemVer) mempty "1.2.0-a.b.c+meta.release.99.100"
