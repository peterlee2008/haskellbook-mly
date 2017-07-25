-- 1.
a = fmap (+1) $ read "[1]" :: [Int]

-- 2.
b = (fmap .fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = fmap (*2) (\x -> x - 2)
-- oh, so functions are functors and the data they contain are fmappable!

-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])


-- 5.
-- No idea, giving up.
{--

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi
    in fmap (*3) changed

--}
