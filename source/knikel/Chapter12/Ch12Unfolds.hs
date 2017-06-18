module Ch12Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = f a : myIterate f (f a)

-- Daayum, that was hard
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  (Just (a', b')) -> a' : myUnfoldr f b'
  Nothing -> []

-- Daayum, that was even harder
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr f' x where
  f' b = Just (f b, f b)
  f' _ = Nothing
