data IntList = Cons Int IntList | Nil

intListProd :: IntList -> Int 
intListProd Nil = 1
-- This case won't compile (type mismatch) and blocks the next case because it matches everything
-- intListProd x = x  
intListProd (x `Cons` l) = x * intListProd l

-- First Attempt
--data BinaryTree = BinaryTree BinaryTree Int BinaryTree 
--                  | EmptyLeaf

--dummyTree :: BinaryTree
--dummyTree = BinaryTree (Leaf 1) 3 (Leaf 2)

--checkElement :: BinaryTree -> Int -> Bool 
--checkElement (Leaf x) n = x == n
--checkElement (BinaryTree l x r) =
--  | x == n = True
--  | otherwise = (checkElement l n) (checkElement r n) -- Not sure how to combine these

data BinaryTree = Empty |
                  Node Int BinaryTree BinaryTree
                  deriving (Show, Eq)

dummyTree :: BinaryTree
dummyTree = Node 3 (Node 1 Empty Empty) (Node 2 Empty Empty)

{- My First Attempt after Tree definition was changed
checkElement :: BinaryTree -> Int -> Bool 
checkElement (Node x l r) n
  | x == n = True
  | l /= Empty = checkElement l n   -- Ran into problem here but `deriving (Show, Eq)` fixes this
  | r /= Empty = checkElement r n
  | otherwise = False
-}

checkElement :: BinaryTree -> Int -> Bool
checkElement Empty _ = False
checkElement (Node x l r) n = x == n || checkElement l n || checkElement r n -- Addresses my "combination problem" above

data RoseTree = RoseNode Int [RoseTree] | RoseEmpty

-- data RoseNode = Int

-- data RoseTree = RoseTreeEmpty | 
--                 RoseNode Int RoseNodeList

-- data RoseNodeList = RoseCons RoseNode RoseNodeList | RoseListEmpty