module Syntax where

type Name = String 

data Type = TVar Name | TArr Type Type deriving (Eq, Ord, Show)
data Expr = Var Name | App Expr Expr | Lam Name Type Expr deriving (Eq, Ord, Show) 

type Env = [(Name, Type)]
extend = (:)
