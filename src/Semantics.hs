module Semantics where
import Syntax

findT :: Env -> Expr -> Maybe Type
findT env (Var x)     = lookup x env >>= Just
findT env (Lam x t e) = findT (extend (x, t) env) e >>= Just . TArr t
findT env (App e1 e2) = do
                    (TArr t1 t2) <- findT env e1 
                    t3           <- findT env e2 
                    if t1 == t3 then Just t2 else Nothing
