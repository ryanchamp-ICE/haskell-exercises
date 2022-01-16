-- I failed to complete this :(
-- But here is my first attempt

-- TODO: Refactor
performMove :: Int -> [[Int]] -> Int -> Int -> [[Int]]
performMove n [x, y, z] a b
  | n <= 0 = []
  | a < 0 || a > 2 = [x, y, z]
  | b < 0 || b > 2 = [x, y, z]
performMove n [[x], y, z] 0 0 = [[x], y, z]
performMove n [[x], y, z] 0 1 = [[], x : y, z]
performMove n [[x], y, z] 0 2 = [[], y, x : z]
performMove n [[], y, z] 0 _ = [[], y, z]
performMove n [x : xs, y, z] 0 0 = [x : xs, y, z]
performMove n [x : xs, y, z] 0 1 = [xs, x : y, z]
performMove n [x : xs, y, z] 0 2 = [xs, y, x : z]
performMove n [x, [], z] 1 _ = [x, [], z]
performMove n [x, [y], z] 1 0 = [y : x, [], z]
performMove n [x, [y], z] 1 1 = [y : x, [y], z]
performMove n [x, [y], z] 1 2 = [x, [y], y : z]
performMove n [x, y : ys, z] 1 0 = [y : x, ys, z]
performMove n [x, y : ys, z] 1 1 = [x, y : ys, z]
performMove n [x, y : ys, z] 1 2 = [x, ys, y : z]
performMove n [x, y, [z]] 2 0 = [z : x, y, []]
performMove n [x, y, [z]] 2 1 = [x, z : y, []]
performMove n [x, y, [z]] 2 2 = [x, y, [z]]
performMove n [x, y, []] 2 _ = [x, y, []]
performMove n [x, y, z : zs] 2 0 = [z : x, y, zs]
performMove n [x, y, z : zs] 2 1 = [x, z : y, zs]
performMove n [x, y, z : zs] 2 2 = [x, y, z : zs]

minInt :: Int
minInt = -999

tail' :: [Int] -> [Int]
tail' [] = []
tail' [x] = [x]
tail' (x:xs) = xs

head' :: [Int] -> Int
head' [] = minInt
head' [x] = x
head' (x:xs) = x

last' :: [Int] -> Int 
last' [] = minInt
last' x = last x

-- This is a generalized refactor of performMove
moveElem :: Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
moveElem i el [] src dest
  | i == src = [[el]]
  | i == dest && el /= minInt = [[el]]
  | otherwise = []
moveElem i el elemList@(x:xs) src dest
  | i == src = tail' x : moveElem (i + 1) (head' x) xs src dest
  | i == dest && el /= minInt = (el:x) : moveElem (i + 1) el xs src dest
  | otherwise = x : moveElem (i + 1) el xs src dest


isValidMove :: [Int] -> [Int] -> Bool
isValidMove _ [] = True
isValidMove [x] [y] = x < y
isValidMove (x : xs) [y] = x < y
isValidMove (x : xs) (y : ys) = x < y

canMove :: [Int] -> Int -> Bool
canMove [] _ = False
canMove (x : xs) disk = x == disk

max' :: Int -> Int -> Int
max' a b
  | a > b = a
  | otherwise = b

maxHead :: [[Int]] -> Int
maxHead [] = 0
maxHead [[]] = 0
maxHead [[x]] = x
maxHead [x : xs] = max' x (maxHead [xs])

min' :: Int -> Int -> Int
min' a b
  | a < b = a
  | otherwise = b

minHead :: [[Int]] -> Int
minHead [] = 0
minHead [x : xs] = min' x (minHead [xs])

maxList :: [Int] -> Int
maxList [] = 0
maxList [x] = x
maxList (x : xs) = max' x (maxList xs)

-- This was my first failed algo attempt
-- This no longer compiles since I changed the signature of performMove
-- buildTower :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
-- buildTower xs 0 _ _ _ = xs
-- buildTower xs 1 source target originalSize = performMove xs source (findTarget xs originalSize)
-- buildTower xs towerSize source target originalSize
--   | not (canMove xs towerSize) = buildTower xs source (towerSize - 1) target originalSize
--   | otherwise = buildTower (performMove xs source target) source (towerSize - 1) target (towerSize - 1)


-----------------------------------------------------------------------
--- I later found a recursive solution to the problem
--- I understand the solution BUT I couldnt get the data represtation right
--- If you run the function below, it will run for the proper number of steps
--- But have the wrong data lol :(
--- I need to rewrite performMove for this to work completely

-- Assuming 3 poles
-- Given the number of disks
-- List all of the steps that move the disks from the source pole to target pole
-- No smaller disk can be on top of a larger disk
-- Initial state [[1, 2, 3, 4, 5], [], []]
-- moveDisks :: Int -> [[[Int]]]

loadDisks :: Int -> [Int]
loadDisks 0 = []
loadDisks n
  | n < 0 = []
  | otherwise = [1..n]

-- There’s a classic recursive solution: move the top (N-1) disks to the spare tower, 
-- then move the large bottom disk to the target tower, 
-- then finally move those (N-1) disk from the spare tower to the target tower.


initialState :: [[Int]]
initialState = [[1,2,3], [], []]

hanoi1 :: Int -> [[Int]] -> [[[Int]]]
hanoi1 0 _ = []
hanoi1 n towerList@[x, y, z] = 
  hanoi1 (n - 1) [x, z, y] ++
  [performMove n [x, y, z] 0 2] ++
  hanoi1 (n - 1) [y, x, z]

-- Attempting to solve by "moving the towers" instead of "moving the disks"
-- With refactor of performMove
hanoi2 :: Int -> [[Int]] -> [[[Int]]]
hanoi2 0 _ = []
hanoi2 n towerList@[x, y, z] = 
  hanoi2 (n - 1) [disksAbove x, z, y] ++
  [moveElem 0 (-999) [[last x], y, z] 0 2] ++
  hanoi2 (n - 1) [y, x, z]

disksAbove :: [Int] -> [Int]
disksAbove [] = []
disksAbove [x] = [x]
disksAbove (x:xs) = init (x:xs)

-- IN PROGRESS
-- Attempting to solve by moving the disks and specifying source and destination
-- The problem here is that we need to build up state during each recursive step
hanoi3 :: [[Int]] -> Int -> Int -> Int -> Int -> [[[Int]]]
hanoi3 _ 0 _ _ _ = []
hanoi3 towerList@[x, y, z] n src spare dest = 
-- Move n-1 disks to spare
  hanoi3 [disksAbove x, y, z] (n - 1) src dest spare ++
-- Move final disk to target
  [moveElem 0 minInt [x, y, z] src dest] ++
-- Move n-1 disks to target
  hanoi3 [x, y, z] (n-1) spare src dest

-- Other solutions below
-----------------------------------------------------------------------------

moveFromTo :: [Int] -> String -> String -> String -> [(Int, String, String)]
moveFromTo [] x y z = []
moveFromTo (d:ds) x y z = (moveFromTo ds x z y) ++ (d,x,y) : moveFromTo ds z y x

--- Adithya (Instructor) ---
insertIntoTower :: Int -> Int -> [[Int]] -> [[Int]]
insertIntoTower 0 i [a, b, c] = [a ++ [i], b, c]
insertIntoTower 1 i [a, b, c] = [a, b ++ [i], c]
insertIntoTower 2 i [a, b, c] = [a, b, c ++ [i]]

-- | map_ basically applies a function to all elements in the list
map_ f [] = []
map_ f (x:xs) = f x : map_ f xs

-- | last takes the last element of the list
last_ [x] = x
last_ (x:xs) = last_ xs

-- | solution to towers of hanoi
towersOfHanoiStep :: Int -> Int -> Int -> [[[Int]]]
towersOfHanoiStep 0 i j = 
    [insertIntoTower i 0 [[], [], []], insertIntoTower j 0 [[], [], []]]
towersOfHanoiStep numDisks i j =
    let stepsBefore = towersOfHanoiStep (numDisks - 1) i (3 - i - j)
        stepsAfter = towersOfHanoiStep (numDisks - 1) (3 - i - j) j
        lastStep = last_ stepsBefore
     in (map_ (insertIntoTower i numDisks) stepsBefore)
            ++ [insertIntoTower j numDisks lastStep]
            ++ (map_ (insertIntoTower j numDisks) stepsAfter)
