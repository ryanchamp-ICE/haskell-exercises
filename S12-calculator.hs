{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

-- data ExprT = Lit Integer
--             | Add ExprT ExprT
--             | Mul ExprT ExprT
--     deriving (Show, Eq)

data VarExprT = Lit Integer
            | Add VarExprT VarExprT
            | Mul VarExprT VarExprT
            | Var String
    deriving (Show, Eq)

eval :: VarExprT -> Integer
eval (Lit i) = i
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

evalMaybe :: Maybe VarExprT -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just (Var _)) = Nothing
evalMaybe (Just expr) = Just $ eval expr

testExp :: VarExprT
testExp = Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- "(2+3)*4"
-- Assuming this should come from somewhere?
parseExp :: String -> Maybe VarExprT
parseExp = undefined

evalStr :: String -> Maybe Integer
evalStr "" = Nothing
evalStr str = evalMaybe $ parseExp str

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit i = i
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit i = i > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

class HasVars a where
    var :: String -> a

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = undefined
    add exp1 exp2 = undefined
    mul exp1 exp2 = undefined