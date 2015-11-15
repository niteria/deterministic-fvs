module Tests where

import Types
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

varRange :: Int -> Int -> [Term]
varRange a b = map Var [a..b]

test1 :: Term
test1 =
  App
    (Var 10)
    [ App
        (Lam 20
          (Lam 15
            (App
              (Var 15)
              [Var 12])))
        (varRange 101 120)
    , Var 201
    , Lam 19
        (App
          (Var 20)
          [ Var 19
          , Lam 18
              (App
                (Var 25)
                (varRange 20 50))
          , App
              (Var 29)
              [Var 43]
          ])
    ]

testF :: Term -> Term
testF hole =
  App
    (Var 10)
    [ App
        (Lam 20
          (Lam 15
            (App
              hole
              [Var 12])))
        (varRange 101 120)
    , Var 201
    , Lam 19
        (App
          (Var 20)
          [ hole
          , Lam 18
              (App
                (Var 25)
                (varRange 20 50))
          , App
              (Var 29)
              [Var 43]
          ])
    ]

leftF :: Term -> Term
leftF hole =
  Lam 10
    (Lam 20
      (App
        hole
        [Var 10, Var 15]))

rightF :: Term -> Term
rightF hole =
  Lam 11
    (Lam 21
      (App
        (Var 11)
        [hole]))

llr :: Term -> Term
llr = leftF . leftF . rightF

rrl :: Term -> Term
rrl = rightF . rightF . leftF

test2 :: Term
test2 = foldr ($) test1 (replicate 4 testF)

test3 :: Term
test3 = foldr ($) test1 (replicate 18 testF)

test4 :: Term
test4 = foldr ($) test1 (replicate 1000 leftF)

test5 :: Term
test5 = foldr ($) test1 (replicate 1000 rightF)

test6 :: Term
test6 = foldr ($) test1 (replicate 1000 llr)

test7 :: Term
test7 = foldr ($) test1 (replicate 1000 rrl)


testMap :: HashMap String Term
testMap = HashMap.fromList
  [ ("test1", test1)
  , ("test2", test2)
  , ("test3", test3)
  , ("test4", test4)
  , ("test5", test5)
  , ("test6", test6)
  , ("test7", test7)
  ]

