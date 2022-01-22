-- Session 2 Exercise
take' :: Int -> [Int] -> [Int]
take' _ [] = []     -- I originally missed this case
take' a (x:xs)
  | a <= 0 = []
  | otherwise = x : take' (a - 1) xs