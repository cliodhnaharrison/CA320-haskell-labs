data Tree a =
  Null | Node a (Tree a) (Tree a)
  deriving (Read, Show)

addNode :: Ord a => a -> Tree a -> Tree a
addNode x Null = Node x Null Null
addNode x (Node n left right)
  | x == n = Node n left right
  | x < n = Node n (addNode x left) right
  | x > n = Node n left (addNode x right)

makeTree :: Ord a => [a] -> Tree a
makeTree [] = Null
makeTree [x] = Node x Null Null
makeTree (x:xs) = addNode x (makeTree xs)
