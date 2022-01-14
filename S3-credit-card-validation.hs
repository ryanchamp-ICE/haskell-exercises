addToEnd :: [Int] -> Int -> [Int]
addToEnd [] y = [y]
addToEnd (x:xs) y = x : addToEnd xs y

-- reverse an int list
reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = addToEnd (reverse' xs) x

-- Multiply Even list elements by 2
doublize :: [Int] -> [Int]
doublize [] = []
doublize [x] = [x]
doublize (x:y:zs) = x : (2 * y) : doublize zs

doublizeFromEnd :: [Int] -> [Int]
doublizeFromEnd ls = doublize (reverse' ls)

-- Numbers less than 19
flattenNumber :: Int -> Int
flattenNumber x =
  if x < 10
    then x
  else x - 9

flattenNumberInList :: [Int] -> [Int]
flattenNumberInList [] = []
flattenNumberInList (x:xs) = flattenNumber x : flattenNumberInList xs

-- Sum the elements of the list
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

checkDiv10 :: Int -> Bool
checkDiv10 x = x `mod` 10 == 0

-- Validate credit card
mod10 :: [Int] -> Bool
mod10 x =
  checkDiv10 (sum' (flattenNumberInList (doublizeFromEnd x)))