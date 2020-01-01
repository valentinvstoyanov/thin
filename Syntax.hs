module Syntax where

type Name = String 
data Term = Var Name | App Term Term | Lam Name Term deriving (Eq, Ord, Show) 
data Type = TVar Name | TArr Type Type deriving (Eq, Ord, Show)


