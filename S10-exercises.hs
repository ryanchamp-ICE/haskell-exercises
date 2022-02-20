-- Implement functions idiomatically (not recursively)
fun1 :: [Int] -> Int
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Int] -> Int
fun1' xs = product $ map (\x -> x - 2) $ filter even xs
-- fun1' = product . map (-2) . filter even

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

-- Did not implement
-- fun2' :: Int -> Int
-- fun2' x =

-- Arthur's Solution
func2 :: Int -> Int
func2 1 = 0
func2 n | even n    = n + func2 (n `div` 2)
        | otherwise = func2 (3 * n + 1)


func2' :: Int -> Int
func2' = sum . filter even . takeWhile (> 1) . iterate
    (\x -> case () of
        _ | even x -> x `div` 2
          | odd x  -> 3 * x + 1
    )

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
isBalanced (Node h left _ right) = abs (height left - height right) <= 1 && isBalanced left && isBalanced right

leftLean :: Tree a -> Bool 
leftLean Leaf = False 
leftLean (Node h left _ right) = height left > height right || leftLean left || leftLean right

-- tilt :: Tree a -> Int
-- tilt Leaf = 0
-- tilt (Node h left _ right) = 
--   | height left - height right == 0

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree input Leaf = Node 1 Leaf input Leaf
insertIntoTree input curNode@(Node h left value right)
  | isBalanced curNode = Node (h + 1) (insertIntoTree input left) value right
  | leftLean curNode = Node h left value (insertIntoTree input right)
  | otherwise = Node h (insertIntoTree input left) value right

insertIntoTree' :: a -> Tree a -> Tree a
insertIntoTree' input Leaf =
    let left = Leaf
        right = Leaf
        h1 = max (height left) (height right) + 1
     in Node h1 left input right
insertIntoTree' input curNode@(Node h left value right)
    | height left > height right =
        let left1 = left
            right1 = insertIntoTree' input right
            h1 = max (height left1) (height right1) + 1
        in Node h1 left1 value right1
    | otherwise =
        let left1 = insertIntoTree' input left
            right1 = right
            h1 = max (height left1) (height right1) + 1
        in Node h1 left1 value right1


foldTree :: [a] -> Tree a
foldTree xs = foldr insertIntoTree' Leaf xs

result = isBalanced (foldTree [1, 2, 3,4 ,5, 6,7, 8, 9])

-- foldl insert Leaf [1,2,3,4]
-- foldl insert (insert 1 Leaf) [2,3,4]
-- foldl insert (Node 1 Leaf 1 Leaf) [2,3,4]
-- foldl insert (insert 2 (Node 1 Leaf 1 Leaf)) [3,4]
-- foldl insert (Node 2 (insert 2 Leaf) 1 Leaf) [3,4]
-- foldl insert (Node 2 (Node 1 Leaf 2 Leaf) 1 Leaf) [3,4]
-- foldl insert (insert 3 (Node 2 (Node 1 Leaf 2 Leaf) 1 Leaf)) [4]
-- foldl insert (Node 2 (Node 1 Leaf 2 Leaf) 1 (insert 3 Leaf)) [4]
-- foldl insert (Node 2 (Node 1 Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)) [4]
-- foldl insert (Node 3 (insert 4 (Node 1 Leaf 2 Leaf) 1 (Node Leaf 3 Leaf))) []
-- foldl insert (Node 3 (Node 2 ((insert 4 Leaf) 2 Leaf)) 1 (Node Leaf 3 Leaf)) []
-- foldl insert (Node 3 (Node 2 ((Node 1 Leaf 4 Leaf) 2 Leaf)) 1 (Node Leaf 3 Leaf)) []
-- Node 3 (Node 2 ((Node 1 Leaf 4 Leaf) 2 Leaf)) 1 (Node Leaf 3 Leaf)

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




-- Implement xor using fold
xor :: [Bool] -> Bool
xor xs = odd $ foldr (\_ i -> i + 1) 0 (filter (== True) xs)

-- xor :: [Bool] -> Bool
-- xor = odd . length . filter (==True)

xor' :: [Bool] -> Bool
xor' = foldr (/=) False 

-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> f x : ys) [] xs

-- Implement foldl using foldr (attempt 2)
-- From wikipedia: 
-- For every function defined as
--  g [] = v
--  g (x:xs) = f x (g xs)
--  g can be defined as:
--  g = foldr f v
-- foldl' f z [] = z
-- foldl' f z (x:xs) = foldl' f (f z x) xs => g (f z x)
-- Hint: think of composition of functions (.)
-- g == foldl' f z
-- v = z
-- f x = (f z x)
-- foldr :: (a -> b -> b)
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f z xs = foldr (\x -> ) z xs


-- Implement Sieve of Sundaram
-- sieveSundaram :: Int -> [Int]
-- Start with a list of the integers from 1 to n.
-- From this list, remove all numbers of the form i + j + 2ij where:
-- 1 <= i <= j
-- i + j + 2ij <= n
-- The remaining numbers are doubled and incremented by one, 
-- giving a list of the odd prime numbers (i.e., all primes except 2) below 2n + 1.
sieveOfSundaram :: Int -> [Int]
sieveOfSundaram n = map (\x -> (2 * x) + 1) $ filter (`notElem` sieveList) [1..n]
  where sieveList = [i + j + (2 * i * j) | i <- [1..n], j <- [i..n], i + j + (2 * i * j) <= n]

-- cartesian Product
