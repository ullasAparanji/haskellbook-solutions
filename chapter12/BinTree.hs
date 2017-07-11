module BinTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case (f a) of
       (Just (x, y, z)) -> Node (unfold f x) y (unfold f z)
       Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> if a == 0 then Nothing else Just (a-1, a-1, a-1)) n
