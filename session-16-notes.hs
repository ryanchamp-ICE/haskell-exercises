-- Semigroups and Monoids

-- Typeclasses Review
-- Set of Types
-- Also defines an interface with functions

-- See Session 11b Notes
-- class Num' a where
--   (+), (*), (-) :: a -> a -> a
--   fromInteger :: Integer -> a

-- data MyNum = Odd | Even

-- instance Num' MyNum where
--   ...

-- For every type class, it's a good idea to summarize what the typeclass represents

-- Semigroups are types with an associative binary operation
-- x <> (y <> z) = (x <> y) <> z
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data ListWithLen a = ListWithLen [a] Int
  deriving Show

instance Semigroup (ListWithLen a) where
  (ListWithLen list1 len1) <> (ListWithLen list2 len2) =
    ListWithLen (list1 <> list2) (len1 + len2)

-- Monoid is a Semigroup with a definition for "Zero"
instance Monoid (ListWithLen a) where
  mempty = ListWithLen [] 0

fromHaskellList :: [a] -> ListWithLen a
fromHaskellList list = ListWithLen list (length list)

data Color = Red 
          | Yellow 
          | Blue 
          | Orange 
          | Green 
          | Violet
          | Brown
          | Clear
  deriving (Show, Eq)

instance Semigroup Color where
  Red <> Yellow = Orange
  Yellow <> Red = Orange
  Red <> Blue = Violet
  Blue <> Red = Violet
  Yellow <> Blue = Green
  Blue <> Yellow = Green
  Green <> x | x == Blue || x == Yellow = Green
  x <> Green | x == Blue || x == Yellow = Green
  Violet <> x | x == Blue || x == Yellow = Violet
  x <> Violet | x == Blue || x == Yellow = Violet
  Orange <> x | x == Blue || x == Yellow = Orange
  x <> Orange | x == Blue || x == Yellow = Orange
  x <> y = Brown

-- Monoid is a Semigroup with a definition for "Zero"
instance Monoid Color where
  mempty = Clear

data Maybe' a = Just' a | Nothing'

instance Semigroup a => Semigroup (Maybe' a) where
  Just' x <> Just' y = Just' (x <> y)
  x <> Nothing' = Nothing'
  Nothing' <> x = Nothing'

-- Monoid is a Semigroup with a definition for "Zero"
instance Semigroup a => Monoid (Maybe' a) where
  mempty = Nothing'

-- Let's add numeric types to Semigroup
-- Define a wrapper type
-- newtype keyword is used to create wrapper types
-- Can only have exactly one constructor, and exactly one field
type Address = String

newtype Name = Name String
  deriving (Eq, Ord, Show)

newtype Phone = Phone Int
  deriving (Eq, Ord, Show, Num)

-- data Phone' = Phone' Int
--   deriving (Eq, Ord, Show, Num)

newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Semigroup (Sum a) where
  -- (Sum x) <> (Sum y) = Sum (x + y)
  -- This + is derived on line 81 when declaring Sum
  (<>) = (+)

-- Monoid is a Semigroup with a definition for "Zero"
instance Num a => Monoid (Sum a) where
  mempty = Sum 0

-- type vs newtype vs data
-- data
-- >= 1 constructor and >= 0 fields
-- for creating ADTs, and actually creates a fresh type

-- Type
-- 0 constructor and 1 field (type)
-- for type aliases; doesn't create a fresh type
-- makes code easier to read by using more domain specific names

-- newtype
-- 1 constructor and 1 field
-- creates a fresh type
-- used to create a wrapper around another type (exactly 1)

newtype Prod a = Prod a
  deriving (Eq, Ord, Num, Show)

-- Exercise: Create a semigroup instance for Num that does multiplication
instance Num a => Semigroup (Prod a) where
  -- (Sum x) <> (Sum y) = Sum (x + y)
  -- This + is derived on line 81 when declaring Sum
  (<>) = (*)

-- Exercise: Create semigroup instances for Bool (one for and, and one for or)
-- instance Semigroup Bool where
--   (<>) = (||)

-- instance Semigroup Bool where
--   (<>) = (&&)

-- Monoid is mostly useful because it enables use of this function
-- Monoid supplies the base case (mempty) for foldr
-- mconcat :: Monoid a => [a] -> a
-- mconcat = foldr mempty (<>)
