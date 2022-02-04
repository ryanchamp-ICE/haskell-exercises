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

-- My solution, Following Matyas' pattern (I had the right idea but messed up on the recursion)
returnEveryIthElement :: Int -> [a] -> [a]
returnEveryIthElement _ [] = []
returnEveryIthElement i (x:xs) = x : returnEveryIthElement i (drop i (x : xs))

skipIterator :: Int -> [a] -> [[a]]
skipIterator i [] = []
skipIterator i (x:xs) = returnEveryIthElement i (x:xs) : skipIterator (i + 1) xs

skips :: [a] -> [[a]]
skips xs = skipIterator 1 xs

-------------------------------------------------------------------------------
-- Matyas Solution --
-- Take every nth element of the list, starting with the first one
-- mTakeNth 3 [
-- mTakeNth :: Int -> [a] -> [a]
-- mTakeNth _ [] = []
-- mTakeNth n (x : xs) = x : mTakeNth n (drop n (x : xs))

-- -- Take the nth elements of the list then go with one fewer elements and n increased by 1
-- mSkipsHelper :: Int -> [a] -> [[a]]
-- mSkipsHelper _ [] = []
-- mSkipsHelper n (x : xs) = (mTakeNth n (x : xs)) : mSkipsHelper (n + 1) xs

-- mSkips :: [a] -> [[a]]
-- mSkips = mSkipsHelper 1
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

-- skips :: [a] -> [[a]]
-- skips xs =
--   let
--     ps = [1..length xs] -- auxiliary list to allow iteration on the input list
--     zs = zip ps xs      -- auxliliar list to indicate the position of each element
--   in
--     map (\p -> [ze | (zp,ze) <- zs, mod zp p == 0]) ps

-----------------------------------------------------------------------------------
-- Adithya's Solution
-- | Indexes the the list from 1
--
-- Example:
-- $ indexedFrom1 "bye"
-- > [(1, 'b'), (2, 'y'), (3, 'e')]
-- indexedFrom1 :: [a] -> [(Int, a)]
-- indexedFrom1 xs = zip [1..length xs] xs

-- -- | Output is a list of lists
-- -- The first list is the same as the input list
-- -- The second list should contain every second element
-- -- The nth list should contain every nth element
-- --
-- -- Example:
-- --
-- -- skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- -- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- -- skips [1]          == [[1]]
-- -- skips [True,False] == [[True,False], [False]]
-- -- skips []           == []
-- skips :: [a] -> [[a]]
-- skips xs =

--     -- [1..length xs] :: [Int]
--     -- filterNth :: Int -> [a]
--     map filterNth [1..length xs]

--     where

--     -- This is the indexed version of the input
--     -- indexedXS :: [(Int, a)]
--     indexedXS = indexedFrom1 xs

--     -- A function that checks if the first element
--     -- of the tuple is a multiple of "i"
--     --
--     -- selectingFunc :: Int -> (Int, b) -> Bool
--     --
--     -- Example:
--     -- selectingFunc 2 (4, 'a') == True
--     -- selectingFunc 2 (3, 'a') == False
--     selectingFunc i x = fst x `mod` i == 0

--     -- Chooses every ith element of the indexedXS
--     --
--     -- filterNth :: Int -> [a]
--     filterNth i =
--         map snd (filter (selectingFunc i) indexedXS)