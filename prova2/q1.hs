data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
    deriving Show

somar :: (a -> b) -> Tree a -> Tree b
somar Leaf = 0
somar (Branch a sub_left  sub_right) = a + somar sub_left + somar sub_right