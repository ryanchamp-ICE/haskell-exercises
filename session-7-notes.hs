import Prelude hiding (Just, Nothing)
data IntList = Nil
              | Cons Int IntList

data IntTree = Leaf
           | Node IntTree Int IntTree
-- data Tree a = Leaf
--             | Node (Tree a) a (Tree a)

data MaybeInt = Nothing | Just Int
                deriving (Show, Eq)

elementStatisfiesCondition :: (Int -> Bool) -> IntTree -> MaybeInt
elementStatisfiesCondition f Leaf = Nothing
elementStatisfiesCondition f (Node l x r)
  | f x = Just x
  | elementStatisfiesCondition f l == Nothing = elementStatisfiesCondition f r
  | elementStatisfiesCondition f r == Nothing = Nothing
  | otherwise = Nothing


elementStatisfiesConditionList :: (Int -> Bool) -> IntList -> MaybeInt
elementStatisfiesConditionList f Nil = Nothing
elementStatisfiesConditionList f (Cons x xs)
  | f x = Just x
  | otherwise = elementStatisfiesConditionList f xs

-- Polymorphic Definition of List
data List a = Empty
             | Cons' a (List a)

-- data Maybe a = Nothing | Just a

-- elementStatisfiesCondition' :: (a -> Bool) -> List a -> Maybe a
-- elementStatisfiesCondition' f Empty _ = Nothing
-- elementStatisfiesCondition' f (Cons x xs)
--   | f x = Just x
--   | otherwise = elementStatisfiesConditionList f xs

-- Filtering function
-- filterIntList :: (Int -> Bool) -> IntList -> IntList
-- filterIntList _ Nil = Nil
-- filterIntList f (Cons x xs)
--   | f x = Cons x (filterIntList f xs)
--   | otherwise = filterIntList f xs

-- -- Polymorphic version of filtering function
-- filterList :: (a -> Bool) -> List a -> List a
-- filterList _ Nil = Nil
-- filterList f (Cons x xs)
--   | f x = Cons x (filterList f xs)
--   | otherwise = filterList f xs

-- -- Mapping function
-- applyToAll :: (Int -> Int) -> IntList -> IntList
-- applyToAll _ Nil = Nil
-- applyToAll f (Cons x xs) = Cons (f x) (applyToAll f xs)

-- applyToAll' :: (a -> a) -> List a -> List a
-- applyToAll' _ Nil = Nil
-- applyToAll' f (Cons x xs) = Cons (f x) (applyToAll' f xs)

-- applyToAll'' :: (a -> b) -> List a -> List b
-- applyToAll'' _ Nil = Nil
-- applyToAll'' f (Cons x xs) = Cons (f x) (applyToAll'' f xs)

{-
IntList ~~~ List Int

List Int -> List Int
List Char -> List Char
List Int -> List Char
-}