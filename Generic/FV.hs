module Generic.FV where

import Types
import FVCommon

foldTerm :: Term -> a -> (Var -> a) -> (a -> a -> a) -> (Var -> a -> a) -> a
foldTerm (Var v) empty embed append delete = embed v
foldTerm (App t tys) empty embed append delete =
  foldTerm t empty embed append delete
  `append`
  foldTerms tys empty embed append delete
foldTerm (Lam v t) empty embed append delete =
  delete v $ foldTerm t empty embed append delete

foldTerms :: [Term] -> a -> (Var -> a) -> (a -> a -> a) -> (Var -> a -> a) -> a
foldTerms (t:ts) empty embed append delete =
  foldTerm t empty embed append delete
  `append`
  foldTerms ts empty embed append delete
foldTerms [] empty embed append delete = empty

fvs :: Term -> [Var]
fvs t = runFV $ foldTerm t noFV oneFV unionFV delFV

funs :: [(String, Term -> [Var])]
funs = [("Generic", fvs)]
