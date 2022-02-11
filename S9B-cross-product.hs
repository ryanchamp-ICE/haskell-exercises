-- Take home assignment
-- Solve only using map and lambdas

-- numbers :: [Int]
-- numbers :: [(Int, String)] -> [Int]
-- numbers xs = map (\x -> fst x) xs

-- -- names :: [String]
-- names :: [(Int, String)] -> [String]
-- names xs = map (\x -> snd x) xs

makeTuple :: Int -> String -> (Int, String)
makeTuple a b = (a, b)

-- result :: [[(Int, String)]]
inputNumbers :: [Int]
inputNumbers = [1, 2, 3]

inputNames :: [String]
inputNames = ["Aras", "Ryan"]

result = [
          [ (1, "Aras"), (1, "Ryan")],
          [ (2, "Aras"), (2, "Ryan")],
          [ (3, "Aras"), (3, "Ryan")]
        ]

crossProductSpecific :: [Int] -> [String] -> [[(Int, String)]]
-- crossProductSpecific [] ys = []
-- crossProductSpecific (x:xs) ys = map (makeTuple x) ys : crossProductSpecific xs ys
crossProductSpecific xs ys = map (\x -> map (makeTuple x) ys) xs

crossProduct :: (a -> b -> c) -> [a] -> [b] ->[[c]]
crossProduct f as bs = map (\a -> map (f a) bs) as