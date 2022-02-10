-- Take home assignment
-- Solve only using map and lambdas

-- numbers :: [Int]
numbers :: [(Int, String)] -> [Int]
numbers xs = map (\x -> fst x) xs

-- names :: [String]
names :: [(Int, String)] -> [String]
names xs = map (\x -> snd x) xs

makeTuple :: Int -> String -> (Int, String)
makeTuple a b = (a, b)

-- result :: [[(Int, String)]]

inputNumbers = [1, 2, 3]
inputNames = ["Aras", "Ryan"]

result = [
          [ (1, "Aras"), (1, "Ryan")],
          [ (2, "Aras"), (2, "Ryan")],
          [ (3, "Aras"), (3, "Ryan")]
        ]

crossProductSpecific :: [Int] -> [String] -> [(Int, String)]
crossProductSpecific xs ys = map (\x y -> makeTuple x y) xs ys
-- crossProduct :: (a -> b -> c) -> [a] -> [b] ->[[c]]