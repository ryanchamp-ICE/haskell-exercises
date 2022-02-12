-- func :: (a -> a) -> a
-- func _ x = x
-- func f x = f x

-- isTheSame :: a -> a -> Bool
-- isTheSame x y
-- Here we need to define a function that would define equality for the polymorphic type

data Ordering' = LT' | GT' | EQ'

isTheSame :: (a -> a -> Ordering') -> a -> a -> Bool
isTheSame ordering x y =
  case ordering x y of
    EQ' -> True
    _ -> False

-- The function that you would pass in there has to return EQ' for isTheSame to return true

-- 1. Type classes
-- 2. Instances
-- 3. Types
-- 4. Polymorphic types

-- Type classes <- Defines the API
-- Instances (Type classes) (Type) <- Implementation of the API