module Set.FV where

import Types
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- Not deterministic, only to approximate current implementation

f :: Term -> (Var -> Bool) -> IntSet
f (Var v) fv_cand
  | fv_cand v = IntSet.singleton v
  | otherwise = IntSet.empty
f (App t tys) fv_cand = f t fv_cand `IntSet.union` fs tys fv_cand
f (Lam v t) fv_cand = IntSet.delete v $ f t fv_cand

fs :: [Term] -> (Var -> Bool) -> IntSet
fs tys fv_cand = IntSet.unions $ map (`f` fv_cand) tys

fvs :: Term -> [Var]
fvs t = IntSet.toList $ f t (const True)

funs :: [(String, Term -> [Var])]
funs = [("Set", fvs)]
