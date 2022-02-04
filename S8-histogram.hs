--- Histogram display functions ---
--- Draw a dot at the proper height if the number has occurred at least {height} times
histogramLine :: [Int] -> Int -> String
histogramLine [] _ = "\n"
histogramLine  (x:xs) height
  | x <= height = ' ' : histogramLine xs height
  | otherwise = '*' : histogramLine xs height

histogramDivider :: [Int] -> String
histogramDivider [] = "\n"
histogramDivider (x:xs) = '=' : histogramDivider xs

histogramLegend :: [Int] -> String
histogramLegend [] = "\n"
histogramLegend (x:xs) = show x ++ histogramLegend xs

--- Draw lines at each occurence height starting from the most frequent
--- Draw the divider and the digit legend at the end
histogramLines :: [Int] -> Int -> String
histogramLines [] _ = ""
histogramLines xs height 
  | height == -2 = histogramLegend [0..9]
  | height == -1 = histogramDivider xs ++ histogramLines xs (height - 1)
  | otherwise = histogramLine xs height ++ histogramLines xs (height - 1)
--- End display functions ---

---- Functions that build up the histogram list ----
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
---- End histogram list functions ----

-- histogram [1,4,5,4,6,6,3,4,2,4,9]
histogram :: [Int] -> String
histogram xs = histogramLines (histogramList xs) (maximum $ histogramList xs)