module Syntax where

newtype VarName = VarName {varNameOf :: String} deriving (Eq, Ord)

data Type = TVar VarName | TArr Type Type deriving (Eq, Ord)
data Expr = Var VarName | App Expr Expr | Lam VarName Expr deriving (Eq, Ord) 

instance Show Type where 
    show (TVar v)     = varNameOf v 
    show (TArr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

instance Show VarName where 
    show v = varNameOf v

instance Show Expr where 
    show (Var v)     = varNameOf v
    show (App e1 e2) = "(" ++ show e1 ++ show e2 ++ ")"
    show (Lam v e)   = "\\" ++ show v ++ "." ++ show e 

