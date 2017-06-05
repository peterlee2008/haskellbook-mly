import Data.List

myIterate :: (a -> a) -> a -> [a]
myIterate f val = val : myIterate f (f val)


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f val = case f val of
  Just (val, nextVal) -> val : myUnfoldr f nextVal
  Nothing -> []


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\z -> Just (z, f z)) x
