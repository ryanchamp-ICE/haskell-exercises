data NonEmptyList a = ConsNE a (NonEmptyList a) | Singleton a
-- data NonEmptyList' a = NonEmptyList' a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (Singleton x) = [x]
nelToList (ConsNE x xs) = x : nelToList xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel [] = Nothing
listToNel [x] = Just (Singleton x)
listToNel (x:xs) = Just (ConsNE x (listToNel xs)) -- Is this possible?

headNEL :: NonEmptyList a -> a
headNEL (Singleton x) = x
headNEL (ConsNE x xs) = x