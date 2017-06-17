module Ch10ScansExercises where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibsN20 = take 20 fibs

fibsFilter100 = takeWhile (< 100) $ fibs

factorialScan :: Int -> [Integer]
factorialScan x = take x $ scanl (*) 1 [1..]
