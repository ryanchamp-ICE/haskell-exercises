greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs
  where gt100 x = x > 100

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' xs = filter (\x -> x > 100) xs

greaterThan100'' :: [Integer] -> [Integer]
greaterThan100'' = \xs -> filter (\x -> x > 100) xs

isGT100 :: Int -> Bool
isGT100 x = x > 100
-- isGT100 = \x -> x > 100

isLT100 :: Int -> Bool
isLT100 x = x < 100
-- isLT100 = \x -> x < 100

-- Syntax desugaring
-- Lambda functions are the core of Haskell
-- sum :: Int -> Int -> Int
-- sum x y = x + y
-- sum x = \y -> x + y
-- sum = \x -> (\y -> x + y) 

-- sum3 :: Int -> Int -> Int -> Int
-- sum3 x y z = x + y + z
-- sum3 x y = \z -> x + y + z
-- sum3 x = \y -> (\z -> x + y + z)
-- sum3 = \x -> (\y -> (\z -> (x + y + z)))
-- sum3 = \x -> \y -> \z -> x + y + z
result :: [Int]
-- result = (\x y z -> [x,2*y,3*z]) 5 6 3
result = (\x -> (\y -> (\z -> [x,2*y,3*z]))) 5 6 3

-- Left associative function application. This is the default
result' :: [Int]
result' = (\x -> (\y -> (\z -> [x,2*y,3*z]))) 5 6 3
-- result' = (((\x -> (\y -> (\z -> [x,2*y,3*z]))) 5) 6) 3 -- Same as line above

-- Parens here make this right associative application
result1 = (\x -> (\y -> (\z -> [x,2*y,3*z]) 5) 6) 3

-- function composition (.)
function :: (b -> c) -> (a -> b) -> (a -> c)
function f g = (\x -> f (g x))
-- function f g = f g         -- my original try
-- function f g x = f g x

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

-- f :: (a,b) -> c
-- x :: a
-- y :: b

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x1, x2) = f x1 x2

-- f :: a -> b -> c
-- x :: (a,b)