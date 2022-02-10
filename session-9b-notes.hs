
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

-- occurrences xs i = length (filter (==i) xs)

foobar' :: [Integer] -> Integer
foobar' xs = sum (map (\x -> 7*x + 2) (filter (> 3) xs))

foobar'' :: [Integer] -> Integer
foobar'' = sum . map (\x -> 7*x + 2) . filter (> 3)

-- Fabio's solution using fold
-- foobar' :: [Int] -> Int
-- foobar' = foldr (\x z -> if x > 3 then z + (7 * x + 2) else z) 0

-- fold :: b -> (a -> b -> b) -> [a] -> b
-- fold z f []     = z
-- fold z f (x:xs) = f x (fold z f xs)

-- sum' :: [Integer] -> Integer
-- sum' xs = fold 0 (+) xs

-- sub' :: [Integer] -> Integer
-- sub' xs = fold (-) 0 xs

-- length' :: [a] -> Int
-- length' xs = fold 0 (\_ b -> b + 1) xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold f z []     = z
fold f z (x:xs) = f x (fold f z xs)

sum' :: [Integer] -> Integer
sum' xs = fold (+) 0 xs

sub' :: [Integer] -> Integer
sub' xs = fold (-) 0 xs

length' :: [a] -> Int
length' xs = fold (\_ b -> b + 1) 0 xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)