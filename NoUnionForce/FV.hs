module NoUnionForce.FV where

import Types
import FVCommon

f :: Term -> FV
f (Var v) fv_cand in_scope acc = oneFV v fv_cand in_scope $! acc
f (App t tys) fv_cand in_scope acc =
  f t fv_cand in_scope $! fs tys fv_cand in_scope $! acc
f (Lam v t) fv_cand in_scope acc = delFV v (f t) fv_cand in_scope $! acc

fs :: [Term] -> FV
fs (t:tys) fv_cand in_scope acc =
  f t fv_cand in_scope $! fs tys fv_cand in_scope $! acc
fs [] fv_cand in_scope acc = acc

fvs :: Term -> [Var]
fvs t = runFV $ f t

funs :: [(String, Term -> [Var])]
funs = [("NoUnionForce", fvs)]
