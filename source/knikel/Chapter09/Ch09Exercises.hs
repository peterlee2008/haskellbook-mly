-- Data.Char
import Data.Char

-- 2
upup :: String -> String
upup xs = filter isUpper xs

-- 3.

capi :: String -> String
capi (x:xs) = toUpper x : xs

-- 4.
capiAll :: String -> String
capiAll [] = []
capiAll (x:xs) = toUpper x : capiAll xs

-- 5.
hCapi :: String -> Char
hCapi xs = head xs


-- Ciphers
-- See Ciphers.hs

-- Writing your own standard functions
-- See Standard.hs
