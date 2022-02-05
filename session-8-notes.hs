-- nameThisFunction :: [a] -> [a]

head :: [a] -> a
head = getFirstElem

getFirstElem :: [a] -> a
getFirstElem (x:_) = x

getLastElem :: [a] -> a
getLastElem [x] = x
getLastElem (_:xs) = getLastElem xs

data Maybe' a = Just' a | Nothing'

safeHead :: [a] -> Maybe' a
safeHead [] = Nothing'
safeHead [x] = Just' x
safeHead (x:xs) = Just' x

tail' :: [a] -> [a]
tail' [] = [] -- This is frowned upon; Prefer Maybe [a]
tail' (x:xs) = xs

-- data NonEmptyList a = ConsNE a (NonEmptyList a) | Singleton a
-- data NonEmptyList' a = NonEmptyList' a [a]

-- nelToList :: NonEmptyList a -> [a]
-- nelToList (Singleton x) = [x]
-- nelToList (ConsNE x xs) = x : nelToList xs

-- listToNel :: [a] -> Maybe (NonEmptyList a)
-- listToNel [] = Nothing
-- listToNel (x:xs) = Just (ConsNE x (listToNel xs))

-- headNEL :: NonEmptyList a -> a
-- headNEL (Singleton x) = x
-- headNEL (ConsNE x xs) = x

-- -- tailNEL :: NonEmptyLis a -> [a]
-- -- tailNEL