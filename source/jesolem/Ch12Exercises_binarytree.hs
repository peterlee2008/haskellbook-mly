
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- Ex 1
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f val = case f val of
  Just (valLeft, nodeVal, valRight) -> Node (unfold f valLeft) nodeVal (unfold f valRight)
  Nothing -> Leaf

-- EX 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0
