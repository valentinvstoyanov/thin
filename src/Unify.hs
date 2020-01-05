module Unify where

import Env
import Subst
import Syntax

-- Type unification
unify :: Type -> Type -> TypeSubst 
unify t tv@(TVar _)             = extendSubst tv t emptySubst 
unify tv@(TVar _) t             = extendSubst tv t emptySubst
unify (TArr t1 t2) (TArr t3 t4) = let substs1 = unify t1 t3 
                                      substs2 = unify (substType t2 substs1) (substType t4 substs1) in
                                      mergeSubsts substs1 substs2
unify t1 t2                     = error $ "Can't unify types: " ++ show t1 ++ " and " ++ show t2 ++ "."
