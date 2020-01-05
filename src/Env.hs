module Env where 

import Syntax 

type Env = [(VarName, Type)]

emptyEnv :: Env 
emptyEnv = []

-- Function that adds a new pair of variable name and type to the environment.
extendEnv :: VarName -> Type -> Env -> Env 
extendEnv v t env = (v, t) : env

-- Function which returns the type of the passed variable name in the environment.
getType :: VarName -> Env -> Type
getType v env = case lookup v env of
                    Just t  -> t 
                    Nothing -> error $ varNameOf v ++ " not found in the environment."

-- Function which returns the names of the types in the environment.
getEnvTypeNames :: Env -> [VarName]
getEnvTypeNames env = getTypeNames $ getEnvTypes env

getEnvTypes :: Env -> [Type]
getEnvTypes = map snd

-- Function that returns the names of all types.
getTypeNames :: [Type] -> [VarName]
getTypeNames []                = []
getTypeNames ((TVar v):xs)     = v : getTypeNames xs
getTypeNames ((TArr t1 t2):xs) = getTypeNames $ t1 : t2 : xs
