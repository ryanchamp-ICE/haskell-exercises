-- Implement foldl
-- This turned out to be wrong... This is essentially foldr :(
-- ryanFoldl' :: (b -> a -> b) -> b -> [a] -> b
-- ryanFoldl' f z [] = z
-- ryanFoldl' f z (x:xs) = ryanFoldl' f (f z x) xs
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

-- Aras/Iker's solution
-- foldl: (b + x) + xs
arasFoldl :: (b -> a -> b) -> b -> [a] -> b
arasFoldl f z []     = z
arasFoldl f z (x:xs) = arasFoldl f (f z x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- foldr' (-) 0 [1,2,3,4,5]
-- (-) 1 (foldr' (-) 0 [2,3,4,5])
-- (-) 2 ((-) 1 (foldr' (-) 0 [3,4,5]))
-- (-) 3 ((-) 2 ((-) 1 (foldr' (-) 0 [4,5])))
-- (((1-(2-(3-(4-(5 - 0))))

-- foldl' (-) 0 [1,2,3,4,5]
-- (-) (foldl' (-) 0 [2,3,4,5]) 1
-- (-) (((-) foldl' (-) 0 [3,4,5]) 2) 1
-- (((((0-5)-4)-3)-2)-1)
-- -- (((1-(2-(3-(4-(5))))
-- -- (((1-(2-(3-(-1))))
-- -- (((1-(2-(4))))
-- -- (((1-(-2))))
-- -- (((3)))

-- z :: b
-- f :: (b -> a -> b)

-- Implement foldl in terms of foldr
-- This is also incorrect
-- foldl'' :: (b -> a -> b) -> b -> [a] -> b
-- foldl'' f z xs = foldr' (\a b -> f b a) z xs
