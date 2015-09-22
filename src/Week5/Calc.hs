
module Week5.Calc where

import Week5.ExprT
import Week5.Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s
            of Just e -> Just $ eval e
               Nothing -> Nothing

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit i = i
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i | i <= 0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a1) (MinMax a2) = MinMax $ max a1 a2
  mul (MinMax a1) (MinMax a2) = MinMax $ min a1 a2

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
