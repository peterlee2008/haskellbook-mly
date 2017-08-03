module Ch17Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdgk"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- liftA3 becomes:
--   [(,,)] <$> [a] <*> [b] <*> [c]
--   [(a,,)] <*> [b] <*> [c]
--   [(a,b,)] <*> [c]
--   [(a,b,c)]
-- Since (,,) has a Monoid instance, this is where the combinations are generated
--
-- NICE!
