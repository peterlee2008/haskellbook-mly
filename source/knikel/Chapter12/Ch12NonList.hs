module Ch12NonList where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  (Just (l, v, r)) -> Node (unfold f l) v (unfold f r)
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold unfoldHelp n where
  unfoldHelp a
    | a == 0 = Nothing
    | otherwise = Just ((a - 1), a, (a -1))
