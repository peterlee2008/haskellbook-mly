module Ch12MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = a : []

cutMaybes :: [Maybe a] -> [a]
cutMaybes [] = []
cutMaybes (Nothing:xs) = cutMaybes xs
cutMaybes (Just x:xs) = x : cutMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = foldr foldHelp (Just []) xs where
  foldHelp (Just a) (Just b) = Just $ a : b
  foldHelp _ _ = Nothing
