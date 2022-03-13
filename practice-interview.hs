import Data.Char

beginsWith :: Char -> String -> Bool
beginsWith _ [] = False
beginsWith c (x:xs) = toLower c == toLower x

-- all' :: (a -> Bool) -> [a] -> Bool
-- all' f [] = True
-- all' f (x:xs)
--   | f x = all' f xs
--   | otherwise = False

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\x _ -> f x) True

type Name = String
type EmployeeId = Int

data Employee = Employee EmployeeId Name
  deriving Show

