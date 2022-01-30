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
