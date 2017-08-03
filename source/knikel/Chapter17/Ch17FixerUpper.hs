module Ch17FixerUpper where

-- 1.
ex1 = const <$> Just "Hello" <*> Just "World"

-- 2.
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
