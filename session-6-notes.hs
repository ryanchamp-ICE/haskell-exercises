data IntList = Cons Int IntList | Nil

--addOneToAll :: IntList -> IntList
--addOneToAll Nil = Nil
--addOneToAll (Cons x xs) = Cons (x+1) (addOneToAll xs)

{-
mult2ToAll :: IntList -> IntList
mult2ToAll Nil = Nil
mult2ToAll (Cons x xs) = Cons (x*2) (mult2ToAll xs)

sub2FromAll :: IntList -> IntList
sub2FromAll Nil = Nil
sub2FromAll (Cons x xs) = Cons (x-2) (sub2FromAll xs)
-}

-- Mapping function
applyToAll :: (Int -> Int) -> IntList -> IntList
applyToAll _ Nil = Nil
applyToAll f (Cons x xs) = Cons (f x) (applyToAll f xs)

addOne :: Int -> Int
addOne x = x + 1

addOneToAll :: IntList -> IntList
addOneToAll x = applyToAll addOne x

mult2ToAll :: IntList -> IntList
mult2ToAll x = applyToAll mult2 x
  where mult2 i = i * 2

sub2FromAll :: IntList -> IntList
sub2FromAll x = applyToAll sub2 x
  where sub2 y = y - 2

absAll :: IntList -> IntList
absAll x = applyToAll abs x

-- keepOnlyPositive :: IntList -> IntList
-- keepOnlyPositive Nil = Nil
-- keepOnlyPositive (Cons x xs)
--   | x > 0 = Cons x (keepOnlyPositive xs)
--   | otherwise = keepOnlyPositive xs

-- keepOnlyEven :: IntList -> IntList
-- keepOnlyEven Nil = Nil
-- keepOnlyEven (Cons x xs)
--   | x `mod` 2 == 0 = Cons x (keepOnlyEven xs)
--   | otherwise = keepOnlyEven xs

-- keepOnlyOdd :: IntList -> IntList
-- keepOnlyOdd Nil = Nil
-- keepOnlyOdd (Cons x xs)
--   | x `mod` 2 /= 0 = Cons x (keepOnlyOdd xs)
--   | otherwise = keepOnlyOdd xs

-- Filtering function
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Nil = Nil
filterIntList f (Cons x xs)
  | f x = Cons x (filterIntList f xs)
  | otherwise = filterIntList f xs

keepOnlyPositive :: IntList -> IntList
keepOnlyPositive x = filterIntList geZero x
  where geZero n = n > 0

keepOnlyEven :: IntList -> IntList
keepOnlyEven x = filterIntList even x

keepOnlyOdd :: IntList -> IntList
keepOnlyOdd x = filterIntList odd x