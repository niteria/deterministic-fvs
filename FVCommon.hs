module FVCommon where

import Types
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)


type InterestingVarFun = Var -> Bool

type FV =
  InterestingVarFun ->
  IntSet ->
  ([Var], IntSet) -> ([Var], IntSet)

unionFV :: FV -> FV -> FV
unionFV fv fv2 p1 p2 acc = fv p1 p2 $! fv2 p1 p2 $! acc

oneFV :: Var -> FV
oneFV v fv_cand in_scope acc@(have, haveSet)
  | IntSet.member v in_scope = acc
  | IntSet.member v haveSet = acc
  | fv_cand v = (v:have, IntSet.insert v haveSet)
  | otherwise = acc

noFV :: FV
noFV _ _ acc = acc

runFV :: FV -> [Var]
runFV fv = fst $ fv (const True) IntSet.empty ([], IntSet.empty)

delFV :: Var -> FV -> FV
delFV v fv fv_cand in_scope acc = fv fv_cand (IntSet.insert v in_scope) acc
