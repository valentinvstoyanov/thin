module Syntax where

type Name = String 

data Expr = Var Name | App Expr Expr | Lam Name Expr deriving (Eq, Ord, Show) 
data Type = TVar Name | TArr Type Type deriving (Eq, Ord, Show)

type Env = [(Name, Type)]

