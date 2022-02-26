import Data.Char

data ExprT a = Lit a
            | Add (ExprT a) (ExprT a)
            | Mul (ExprT a) (ExprT a)
    deriving (Show, Eq)

eval :: ExprT Int -> Int
eval (Lit i) = i
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

evalMaybe :: Maybe (ExprT Int) -> Maybe Int 
evalMaybe Nothing = Nothing 
evalMaybe (Just expr) = Just $ eval expr

testExp :: ExprT Int
testExp = Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- "(2+3)*4"

parseExp :: String -> Maybe (ExprT Int)
parseExp = undefined 
-- parseExp "" = Nothing 
-- parseExp (x:xs)
--  | isDigit x = Lit x : parseExp
--  | 

evalStr :: String -> Maybe Int
evalStr "" = Nothing 
evalStr str = evalMaybe $ parseExp str

class Expr a where
    lit :: a -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- instance (Num a) => Expr (ExprT a) where
--     lit (Lit x) = x
--     add x y = x + y
--     mul x y = x * y