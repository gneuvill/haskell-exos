{-# LANGUAGE FlexibleInstances #-}

module Week5.Calc where

import Week5.ExprT
import Week5.Parser
import qualified Week5.StackVM as SVM
import qualified Data.Map as M

-- Exo 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Exo 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s
            of Just e -> Just $ eval e
               Nothing -> Nothing

-- Exo 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Exo 4
  
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

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 i1) (Mod7 i2) = Mod7 $ (i1 + i2) `mod` 7
  mul (Mod7 i1) (Mod7 i2) = Mod7 $ (i1 * i2) `mod` 7

-- Exo 5

instance Expr SVM.Program where
  lit = (:[]) . SVM.PushI
  add p1 p2 = p1 ++ p2 ++ [SVM.Add]
  mul p1 p2 = p1 ++ p2 ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

-- Exo 6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add f1 f2 = combine (+) f1 f2
  mul f1 f2 = combine (*) f1 f2

combine :: (s -> s -> s)
        -> (t -> Maybe s)
        -> (t -> Maybe s)
        -> t -> Maybe s
combine op f f' m = case (f m, f' m) of
  (Just i1, Just i2) -> Just $ i1 `op` i2
  _ -> Nothing
                       
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
