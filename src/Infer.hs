module Infer(inferExprType, inferType) where

import Syntax
import Env 
import Subst 
import Unify
import Parser 

-- Inferes the type of the given expression in the string.
inferType :: String -> Type 
inferType s = inferExprType $ parseExpr s

-- Inferes the type of the given expression.
inferExprType :: Expr -> Type
inferExprType e = t
    where (t, _, _) = inferExprType' emptyEnv (VarName "") e

-- Helper function that takes environment, previously generated variable name 
-- and the expression whose type needs to be inferred. It then return tripple of 
-- the inferred type, the last generated variable name and the substitutions list. 
-- Note: the variable name is used as a state to generate new variable names.
inferExprType' :: Env -> VarName -> Expr -> (Type, VarName, TypeSubst)
inferExprType' env vState (Var v)   = (getType v env, vState, emptySubst) 
inferExprType' env vState (Lam v e) = let vn                = nextEnvVarName vState env  
                                          tv                = TVar vn
                                          (et, evState, es) = inferExprType' (extendEnv v tv env) vn e in 
                                          (TArr (substType tv es) et, evState, es)
inferExprType' env vState (App f e) = let (ft, fvState, fs) = inferExprType' env vState f 
                                          (et, evState, es) = inferExprType' (substEnv env fs) fvState e
                                          vn                = nextEnvVarName evState (substEnv env es)
                                          tv                = (TVar vn) 
                                          us                = unify (substType ft es) (TArr et tv) in 
                                          (substType tv us, vn, mergeSubsts us $ mergeSubsts fs es)

-- Generates a new variable name given previously generated one and the environment 
-- to which the new variable name should be unique. Given the environment the 
-- function extracts all type names in the environment and calls nextVarName.
nextEnvVarName :: VarName -> Env -> VarName
nextEnvVarName v env = fst $ nextVarName v $ getEnvTypeNames env

-- Does the real job of generating new variable name given previously generated one and 
-- list of variable names to which the new one should be unique.
nextVarName :: VarName -> [VarName] -> (VarName, [VarName])
nextVarName v vs = (nvn, nvn : vs)
    where nvn = VarName s
          s = nextStr (varNameOf v) (map varNameOf vs)
          nextStr :: String -> [String] -> String 
          nextStr [] xs = nextStr "?`" xs
          nextStr x xs  = nx
              where nx     = if elem genx xs then nextStr nx xs else genx 
                    genx   = (init x) ++ suffix
                    suffix = if lastx == 'z' then "za" else [nextLetter lastx]
                    lastx  = last x
                    nextLetter :: Char -> Char 
                    nextLetter 'z' = 'a'
                    nextLetter l   = succ l
