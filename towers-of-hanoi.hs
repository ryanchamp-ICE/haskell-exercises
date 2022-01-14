-- swapHeads :: [[Int]] -> [[Int]]-> [[Int]]
-- swapHeads [] [] = []
-- swapHeads [src] [dest] = [prependHead dest $ removeHead src]

-- removeHead :: [Int] -> Int
-- removeHead (x:xs) = x

-- prependHead :: [Int] -> Int -> [Int]
-- prependHead [] x = [x]
-- prependHead x y = y:x


-- TODO: Refactor
performMove :: [[Int]] -> Int -> Int -> [[Int]]
performMove [x, y, z] a b
  | a < 0 || a > 2 = [x, y, z]
  | b < 0 || b > 2 = [x, y, z]
-- performMove (x:xs) srcIndex destIndex = swapHeads [x !! srcIndex] [x !! destIndex]
performMove [[x], y, z] 0 0 = [[x], y, x:z]
performMove [[x], y, z] 0 1 = [[], x:y, z]
performMove [[x], y, z] 0 2 = [[], y, x:z]
performMove [x:xs,y,z] 0 0 = [x:xs, y, z]
performMove [x:xs,y,z] 0 1 = [xs, x:y, z]
performMove [x:xs,y,z] 0 2 = [xs, y, x:z]
performMove [x, [y], z] 1 0 = [y:x, [], z]
performMove [x, [y], z] 1 1 = [y:x, [y], z]
performMove [x, [y], z] 1 2 = [x, [y], y:z]
performMove [x, y:ys, z] 1 0 = [y:x, ys, z]
performMove [x, y:ys, z] 1 1 = [x, y:ys, z]
performMove [x, y:ys, z] 1 2 = [x, ys, y:z]
performMove [x, y, [z]] 0 0 = [z:x, y, []]
performMove [x, y, [z]] 0 1 = [x, z:y, []]
performMove [x, y, [z]] 0 2 = [x, y, [z]]
performMove [x, y, z:zs] 2 0 = [z:x, y, zs]
performMove [x, y, z:zs] 2 1 = [x, z:y, zs]
performMove [x, y, z:zs] 2 2 = [x, y, z:zs]

isValidMove :: [Int] -> [Int] -> Bool
isValidMove _ [] = True
isValidMove [x] [y] = x < y
isValidMove (x:xs) [y] = x < y
isValidMove (x:xs) (y:ys) = x < y

moveDisk :: [[Int]] -> Int -> Int -> [[Int]]
moveDisk xs src dest
  -- | isValidMove (xs !! src) [] = performMove xs src dest
  | isValidMove (xs !! src) (xs !! dest) = performMove xs src dest
  | otherwise = xs

-- Assuming 3 poles
-- Given the number of disks
-- List all of the steps that move the disks from the source pole to target pole
-- No smaller disk can be on top of a larger disk
-- Initial state [[1, 2, 3, 4, 5], [], []]
-- moveDisks :: Int -> [[[Int]]]
initialState :: [[Int]]
initialState = [[1, 2, 3, 4, 5], [], []]
-- moveDisks :: Int -> [[[Int]]]
-- moveDisk target
