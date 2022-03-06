data Tree a = Node (Tree a) a (Tree a)
            | Empty
  deriving Show

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeTest1 :: Tree Int
treeTest1 = Empty

treeTest2 :: Tree Int
treeTest2 = Node Empty 42 Empty

treeTest3 :: Tree Int
treeTest3 = Node treeTest2 18 (Node Empty 39 treeTest2)

treeTest4 :: Tree Int
treeTest4 = Node (Node Empty 11 treeTest3) 50 (Node treeTest2 100 Empty)

tests = [treeTest1, treeTest2, treeTest3, treeTest4]

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node left _ right) = treeSize left + 1 + treeSize right

treeSum :: Tree Int -> Int
treeSum Empty = 0
treeSum (Node left value right) = treeSum left + value + treeSum right

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node left _ right) = 1 + max (treeDepth left) (treeDepth right)

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node left value right) = flatten left ++ [value] ++ flatten right

-- Common Points
-- All have a base case
-- get some result recursively from left and right trees
-- combine the result with the current node in some way via a function

-- User Inputs:
-- Base case value
-- combine function

-- Input :: Tree a
-- Result :: b
-- base case value :: b
-- current node value :: a
-- combine function :: b -> a -> b -> b
foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree z f Empty = z
foldTree z f (Node left v right) = -- f (foldTree z f left) value (foldTree z f right)
  let leftResult = foldTree z f left
      rightResult = foldTree z f right
  in f leftResult v rightResult

treeSize' :: Tree a -> Int
treeSize' = foldTree 0 (\leftSize _ rightSize -> leftSize + 1 + rightSize)

treeSum' :: Tree Int -> Int
treeSum' = foldTree 0 (\leftSum value rightSum -> leftSum + value + rightSum)

treeDepth' :: Tree a -> Int
treeDepth' = foldTree 0 (\leftDepth _ rightDepth -> 1 + max leftDepth rightDepth)

flatten' :: Tree a -> [a]
flatten' = foldTree [] (\flatLeft value flatRight -> flatLeft ++ [value] ++ flatRight)

-- How to create an abstraction?
-- Identify the common patterns
-- Identify what are the most basic inputs that the user must specify

-- Fold for general "bottom up" recursive types
-- One argument for each character
-- All of them are functions that return b
-- Recursive references to your type lead to b input argument
-- Other types remain as is

data Foo a = P a                -- a -> b
           | Q (Foo a)          -- b -> b
           | R a a (Foo a)      -- a -> a -> b -> b
           | S                  -- b
           | T                  -- b

-- NOT AN EXERCISE
foldFoo :: (a -> b) -> (b -> b) -> (a -> a -> b -> b) -> b -> b -> Foo a -> b
foldFoo = undefined

-- Whiteboard: https://excalidraw.com/#json=fY-DtS0XpNTevTkzdfKmE,05rOTc5k967U7FzlfmtHkg