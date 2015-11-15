module EtaReduced.FV where

import Types
import FVCommon

f :: Term -> FV
f (Var v) = oneFV v
f (App t tys) = f t `unionFV` fs tys
f (Lam v t) = delFV v (f t)

fs :: [Term] -> FV
fs (t:tys) = f t `unionFV` fs tys
fs [] = noFV

fvs :: Term -> [Var]
fvs t = runFV $ f t

funs :: [(String, Term -> [Var])]
funs = [("EtaReduced", fvs)]
