histogramLine :: [Int] -> Int -> String
histogramLine [] _ = "\n"
histogramLine  (x:xs) len
  | x <= len = ' ' : histogramLine xs len
  | otherwise = '*' : histogramLine xs len

histogramDivider :: [Int] -> String
histogramDivider [] = "\n"
histogramDivider (x:xs) = '=' : histogramDivider xs

histogramLegend :: [Int] -> String
histogramLegend [] = "\n"
histogramLegend (x:xs) = show x ++ histogramLegend xs

histogramLines :: [Int] -> Int -> String
histogramLines [] _ = ""
histogramLines xs len 
  | len == -2 = histogramLegend [0..9]
  | len == -1 = histogramDivider xs ++ histogramLines xs (len - 1)
  | otherwise = histogramLine xs len ++ histogramLines xs (len - 1)

-- Function that returns the number of times that an element occurs in a list
occurrences :: [Int] -> Int -> Int
occurrences [] _  = 0
occurrences (x:xs) i 
  | i == x = 1 + occurrences xs i 
  | otherwise = occurrences xs i

-- Function that takes an int list and returns the number of occurences for each number in the int list
histogramList :: [Int] -> [Int]
histogramList [] = []
histogramList xs = map (occurrences xs) [0..9]

-- histogram [1,4,5,4,6,6,3,4,2,4,9]
histogram :: [Int] -> String
histogram xs = histogramLines (histogramList xs) (maximum $ histogramList xs)