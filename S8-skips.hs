{-

Output is a list of lists
The first list is the same as the input list
The second list should contain every second element
The nth list should contain every nth element

Example:

skips "ABCD"       == ["ABCD", "BD", "C", "D"]
skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1]          == [[1]]
skips [True,False] == [[True,False], [False]]
skips []           == []

-}

-- Today I learned that basing your solution on !! is probably a smell
-- returnEveryIthElement :: Int -> Int -> [a] -> [a]
-- returnEveryIthElement _ [] = []
-- returnEveryIthElement i j list@(x:xs)
--   | i > length list - 1 || i <= 0 = x:xs
--   | otherwise = list !! i : returnEveryIthElement i (i + j) xs

-- returnEveryIthElement :: Int -> [a] -> [a]
-- returnEveryIthElement _ [] = []
-- returnEveryIthElement i (x:xs) = 


-- skipIterator :: Int -> [a] -> [a]
-- skipIterator i [] = []
-- skipIterator i (x:xs) = skips i x

-- skips :: [a] -> [[a]]
-- skips [] = []
-- skips (x:xs) = returnEveryIthElement 0 [x] : skips xs

-------------------------------------------------------------------------------
-- Matyas Solution --
-- Take every nth element of the list, starting with the first one
-- mTakeNth 3 [
mTakeNth :: Int -> [a] -> [a]
mTakeNth _ [] = []
mTakeNth n (x : xs) = x : mTakeNth n (drop n (x : xs))

-- Take the nth elements of the list then go with one fewer elements and n increased by 1
mSkipsHelper :: Int -> [a] -> [[a]]
mSkipsHelper _ [] = []
mSkipsHelper n (x : xs) = (mTakeNth n (x : xs)) : mSkipsHelper (n + 1) xs

mSkips :: [a] -> [[a]]
mSkips = mSkipsHelper 1
----------------------------------------------------------------------------------
-- Fabio's Solution
-- Fabio Correa
-- module Skips (
--   skips
-- ) where

-- Here the strategy is to build up a list of tuples of the form (Index, Element)
-- where Element is the ith member of the input list
-- Then map a filtering function that you map over the list of indices 
-- to return the elements where their corresponding index in the tuple 
-- mod the index you are looking for == 0

skips :: [a] -> [[a]]
skips xs =
  let
    ps = [1..length xs] -- auxiliary list to allow iteration on the input list
    zs = zip ps xs      -- auxliliar list to indicate the position of each element
  in
    map (\p -> [ze | (zp,ze) <- zs, mod zp p == 0]) ps