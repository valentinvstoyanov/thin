module Env where 

import Syntax 

type Env = [(VarName, Type)]

emptyEnv :: Env 
emptyEnv = []

extendEnv :: VarName -> Type -> Env -> Env 
extendEnv v t env = (v, t) : env

getType :: VarName -> Env -> Type
getType v env = case lookup v env of
                    Just t  -> t 
                    Nothing -> error $ varNameOf v ++ " not found in the environment."

getEnvTypeNames :: Env -> [VarName]
getEnvTypeNames env = getTypeNames $ getEnvTypes env

getEnvTypes :: Env -> [Type]
getEnvTypes = map snd

getTypeNames :: [Type] -> [VarName]
getTypeNames []                = []
getTypeNames ((TVar v):xs)     = v : getTypeNames xs
getTypeNames ((TArr t1 t2):xs) = getTypeNames $ t1 : t2 : xs
