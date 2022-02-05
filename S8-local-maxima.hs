-- Finds all local maxima in the input list
-- returns them in the order that they are found
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
-- localMaxima x y z == x > y && x > z

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x,y] = []
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (y:z:zs)
  | otherwise = localMaxima (y:z:zs)