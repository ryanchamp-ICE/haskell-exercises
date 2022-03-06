import Data.Char

-- twoPower :: Int -> Int
twoPower :: Int -> Int
twoPower n
  | even n = maximum powerList
  | otherwise = -1
  where powerList = takeWhile (\x -> n `mod` 2 ^ x == 0 && 2 ^ x <= n) [1..n]

-- collatz :: Int -> [Int]
collatz :: Int -> [Int]
collatz 1 = []
collatz n
  | even n = n `div` 2 : collatz (n `div` 2)
  | otherwise = 3 * n + 1 : collatz (3 * n + 1)

-- Solved using Sieve of Sundaram algorithm
-- https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- primes :: Int -> [Int]
primes :: Int -> [Int]
primes n
  | n < 2 = []
  | otherwise = 2 : filter (<= n) (map (\x -> (2 * x) + 1) $ filter (`notElem` sieveList) [1..n])
  where sieveList = [i + j + (2 * i * j) | i <- [1..n], j <- [i..n], i + j + (2 * i * j) <= n]

encode :: Eq a => [a] -> [(a, Int)]
encode [] = []
encode (x:xs) = (x, length (takeWhile (== x) (x:xs))) : encode (dropWhile (== x) xs)


caeser :: Int -> String -> String
caeser i = map (nextChar i)
  where nextChar i c = chr (((i + ord (toLower c) - ord 'a') `mod` 26) + ord 'a')