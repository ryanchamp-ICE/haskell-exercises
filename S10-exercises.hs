-- Implement functions idiomatically (not recursively)
fun1 :: [Int] -> Int
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Int] -> Int
fun1' xs = product $ map (\x -> x - 2) $ filter even xs

fun2 :: Int -> Int
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- fun2 3
-- fun2 (3 * 3 + 1) => fun 10
-- 10 + fun2 5
-- 10 + fun2 (3 * 5 + 1) => 10 + fun2 (16)
-- 10 + 16 + fun2 8
-- 10 + 16 + 8 + fun2 4
-- 10 + 16 + 8 + 4 + fun2 2
-- 10 + 16 + 8 + 4 + 2 + fun2 1
-- 10 + 16 + 8 + 4 + 2 + 0 => 40

-- fun2' :: Int -> Int
-- fun2' x =

-- Folding with trees
-- Folding a list of elements to build a balanced binary tree
data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Int
height Leaf = 0
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node h left _ right) = height left == height right && isBalanced left && isBalanced right

-- tilt :: Tree a -> Int
-- tilt Leaf = 0
-- tilt (Node h left _ right) = 
--   | height left - height right == 0

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree input Leaf = Node 1 Leaf input Leaf
insertIntoTree input curNode@(Node h left value right)
  | isBalanced curNode = Node (h + 1) (insertIntoTree input left) value right
  | height left > height right = Node h left value (insertIntoTree input right)
  | otherwise = Node h (insertIntoTree input left) value right

foldTree :: [a] -> Tree a
foldTree xs = foldr insertIntoTree Leaf xs

-- foldr insert Leaf [1,2,3,4,5,6,7]
-- insert 1 (insert 2 (insert 3 (insert 4 (insert 5 (insert 6 (insert 7 (foldr insert Leaf [])))))))
-- insert 1 (insert 2 (insert 3 (insert 4 (insert 5 (insert 6 (insert 7 []))))))
-- insert 1 (insert 2 (insert 3 (insert 4 (insert 5 (insert 6 (Node 1 Leaf 7 Leaf)))))
-- insert 1 (insert 2 (insert 3 (insert 4 (insert 5 (Node 2 (insert 6 Leaf) 7 Leaf))))
-- insert 1 (insert 2 (insert 3 (insert 4 (insert 5 (Node 2 (Node 1 Leaf 6 Leaf) 7 Leaf))))
-- insert 1 (insert 2 (insert 3 (insert 4 (Node 2 (Node 1 Leaf 6 Leaf) 7 (insert 5 Leaf)))))
-- insert 1 (insert 2 (insert 3 (insert 4 (Node 2 (Node 1 Leaf 6 Leaf) 7 (Node 1 Leaf 5 Leaf)))))
-- insert 1 (insert 2 (insert 3 (Node 3 (insert 4 (Node 1 Leaf 6 Leaf)) 7 (Node 1 Leaf 5 Leaf)))
-- insert 1 (insert 2 (insert 3 (Node 3 (Node 2 (insert 4 Leaf) 6 Leaf) 7 (Node 1 Leaf 5 Leaf)))
-- insert 1 (insert 2 (insert 3 (Node 3 (Node 2 (Node 1 Leaf 4 Leaf) 6 Leaf) 7 (Node 1 Leaf 5 Leaf)))
-- insert 1 (insert 2 (Node 3 (Node 2 (Node 1 Leaf 4 Leaf) 6 Leaf) 7 (insert 3 (Node 1 Leaf 5 Leaf)))
-- insert 1 (insert 2 (Node 3 (Node 2 (Node 1 Leaf 4 Leaf) 6 Leaf) 7 (Node 2 (insert 3 Leaf) 5 Leaf))
-- insert 1 (insert 2 (Node 3 (Node 2 (Node 1 Leaf 4 Leaf) 6 Leaf) 7 (Node 2 (Node 1 Leaf 3 Leaf) 5 Leaf))
-- insert 1 (insert 2 (Node 3 (Node 2 (Node 1 Leaf 4 Leaf) 6 Leaf) 7 (Node 2 (Node 1 Leaf 3 Leaf) 5 Leaf))

-- foldr insert Leaf [1,2,3,4]
-- insert 1 (foldr insert Leaf [2,3,4])
-- insert 1 (insert 2 (foldr insert Leaf [3,4]))
-- insert 1 (insert 2 (insert 3 (foldr insert Leaf [4])))
-- insert 1 (insert 2 (insert 3 (insert 4 (foldr insert Leaf []))))  
-- insert 1 (insert 2 (insert 3 (insert 4 [])))
-- insert 1 (insert 2 (insert 3 (Node 1 Leaf 4 Leaf)))
-- insert 1 (insert 2 (Node (1+1) (insert 3 Leaf) 4 Leaf))
-- insert 1 (insert 2 (Node 2 (Node 1 Leaf 3 Leaf) 4 Leaf))
-- insert 1 (Node 2 (Node 1 Leaf 3 Leaf) 4 (insert 2 Leaf))
-- insert 1 (Node 2 (Node 1 Leaf 3 Leaf) 4 (Node 1 Leaf 2 Leaf))
-- Node (2+1) (insert 1 (Node 1 Leaf 3 Leaf) 4 (Node 1 Leaf 2 Leaf))
-- Node 3 (Node (1+1) (insert 1 Leaf) 3 Leaf) 4 (Node 1 Leaf 2 Leaf)
-- Node 3 (Node 2 (Node 1 Leaf 1 Leaf) 3 Leaf) 4 (Node 1 Leaf 2 Leaf)


-- Implement xor using fold
-- xor :: [Bool] -> Bool

-- Implement map as a fold
-- map' :: (a -> b) -> [a] -> [b]

-- Implement foldl using foldr

-- Implement Sieve of Sundaram
-- sieveSundaram :: Int -> [Int]

-- cartesian Product
