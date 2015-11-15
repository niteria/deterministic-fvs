module NoUnionStrictTuple.FV where

import Types
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

type InterestingVarFun = Var -> Bool

data StrictTuple = StrictTuple ![Var] !IntSet

stFst :: StrictTuple -> [Var]
stFst (StrictTuple vs _) = vs

type FV =
  InterestingVarFun ->
  IntSet ->
  StrictTuple -> StrictTuple

unionFV :: FV -> FV -> FV
unionFV fv fv2 p1 p2 acc = fv p1 p2 $! fv2 p1 p2 $! acc

oneFV :: Var -> FV
oneFV v fv_cand in_scope acc@(StrictTuple have haveSet)
  | IntSet.member v in_scope = acc
  | IntSet.member v haveSet = acc
  | fv_cand v = StrictTuple (v:have) (IntSet.insert v haveSet)
  | otherwise = acc

noFV :: FV
noFV _ _ acc = acc

runFV :: FV -> [Var]
runFV fv = stFst $ fv (const True) IntSet.empty (StrictTuple [] IntSet.empty)

delFV :: Var -> FV -> FV
delFV v fv fv_cand in_scope acc = fv fv_cand (IntSet.insert v in_scope) $! acc

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
funs = [("NoUnionStrictTuple", fvs)]
