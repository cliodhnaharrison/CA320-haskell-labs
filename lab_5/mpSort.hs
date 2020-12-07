data Tree a =
  Null | Node a (Tree a) (Tree a)
  deriving (Read, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Null = Node x Null Null
insert x (Node n left right)
  | x == n = Node n left right
  | x < n = Node n (insert x left) right
  | x > n = Node n left (insert x right)

makeTree :: Ord a => [a] -> Tree a
makeTree [] = Null
makeTree [x] = Node x Null Null
makeTree (x:xs) = insert x (makeTree xs)

inOrder :: Tree a -> [a]
inOrder Null = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

mpSort :: Ord a => [a] -> [a]
mpSort [] = []
mpSort x = inOrder (makeTree x)
