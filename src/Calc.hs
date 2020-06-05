{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified Data.Map as M
import Control.Applicative

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>=0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Ord MinMax where
    compare (MinMax a) (MinMax b) = compare a b

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr Mod7 where
    lit = Mod7 . flip mod 7
    add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7


class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
                | VAdd VarExprT VarExprT
                | VMul VarExprT VarExprT
                | Var String
                deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    add f1 f2 m = liftA2 (+) (f1 m) (f2 m)
    mul f1 f2 m = liftA2 (*) (f1 m) (f2 m)


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
-- withVars [("x", 6)] $ add (lit 3) (var "x")
