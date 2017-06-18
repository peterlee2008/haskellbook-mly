module Ch12EitherLib where

lefts' :: [Either a b] -> [a]
lefts' (Left x:xs) = x : lefts' xs
lefts' (_:xs) = lefts' xs

lefts'' :: [Either a b] -> [a]
lefts'' xs = foldr foldHelp [] xs where
  foldHelp (Left a) b = a : b
  foldHelp _ b = b

rights' :: [Either a b] -> [b]
rights' (Right x:xs) = x : rights' xs
rights' (_:xs) = rights' xs

rights'' :: [Either a b] -> [b]
rights'' xs = foldr foldHelp [] xs where
  foldHelp (Right a) b = a : b
  foldHelp _ b = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr foldHelp ([], []) xs where
  foldHelp (Left a) (ls, rs) = (a : ls, rs)
  foldHelp (Right a) (ls, rs) = (ls, a : rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right e) = Just $ f e

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left e) = fl e
either' _ fr (Right e) = fr e

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right e) = Just $ f e
