{-# LANGUAGE FlexibleInstances #-}
data MyNum = Zero | Even | Odd

instance Show MyNum where
  show Zero = "Zero"
  show Even = "Even"
  show Odd = "Odd"

instance Num MyNum where
  Zero + x = x
  Even + x = x
  Odd + Odd = Even
  Odd + Even = Odd
  Odd + Zero = Odd
  Zero * _ = Zero
  _ * Zero = Zero
  Even * _ = Even
  _ * Even = Even
  Odd * Odd = Odd
  abs x = x
  signum Zero = Zero
  signum _ = Odd
  fromInteger 0 = 0
  fromInteger x
    | even x = Even
    | otherwise = Odd
  negate x = x

-- class Listable a where
--   toList :: a -> [a]
--   toList x = [x]

-- instance Listable Bool

-- instance Listable Int

-- instance Listable MyNum where
--   toList Zero = [0]
--   toList Odd = [1]
--   toList Even = [2]

-- instance Listable [Int] where
--   -- toList :: [Int] -> [Int]
--   toList x = [x]

-- instance (Listable a, Listable b) => Listable (a, b) where
--   toList (x, y) = toList x ++ toList y

-- Listable typeclasses represents types that
-- can be converted to a list of Int.

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable MyNum where
  toList Zero = [0]
  toList Odd = [1]
  toList Even = [2]

instance Listable [Int] where
  -- toList :: [Int] -> [Int]
  toList x = x

instance (Listable a, Listable b) => Listable (a, b) where
  -- toList :: (a, b) => [Int]
  toList (x, y) = toList x ++ toList y

sumL :: Listable a => a -> Int
sumL x = sum (toList x)
-- x :: a
-- toList x :: [Int]
-- sum (toList x) :: Int

-- Exercise:
data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving Show
-- Hint: Add a constraint on a.
instance Listable a => Listable (Tree a) where
  -- toList :: Tree a -> [Int]
  toList Empty = [0] -- Could also go with []
  toList (Node left x right) = toList left ++ toList x ++ toList right