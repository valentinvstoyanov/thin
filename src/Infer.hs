module Infer(inferExprType, inferType) where

import Syntax
import Env 
import Subst 
import Unify
import Parser 

inferType :: String -> Type 
inferType s = inferExprType $ parseExpr s

inferExprType :: Expr -> Type
inferExprType e = t
    where (t, _, _) = inferExprType' emptyEnv (VarName "") e

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

nextEnvVarName :: VarName -> Env -> VarName
nextEnvVarName v env = fst $ nextVarName v $ getEnvTypeNames env

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
