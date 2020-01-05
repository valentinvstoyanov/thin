module Subst where 
import Syntax 
import Env

type TypeSubst = [Type -> Type]

emptySubst :: TypeSubst 
emptySubst = []

extendSubst :: Type -> Type -> TypeSubst -> TypeSubst 
extendSubst ft tt substs = (\t -> (if t == ft then tt else t)) : substs

substType :: Type -> TypeSubst -> Type 
substType t substs = foldr (\sub t -> sub t) t substs

substEnv :: Env -> TypeSubst -> Env 
substEnv env substs = foldr (\sub e -> applyEnvSubst sub e) env substs
    where applyEnvSubst :: (Type -> Type) -> Env -> Env 
          applyEnvSubst sub e = map (\(x, t) -> (x, sub t)) e

mergeSubsts :: TypeSubst -> TypeSubst -> TypeSubst 
mergeSubsts = (++)
