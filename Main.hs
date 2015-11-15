module Main where

import System.Environment
import Tests
import Types
import qualified Generic.FV as Generic
import qualified NoUnion.FV as NoUnion
import qualified NoUnionStrictTuple.FV as NoUnionStrictTuple
import qualified NoUnionForce.FV as NoUnionForce
import qualified NoUnionLazy.FV as NoUnionLazy
import qualified EtaExpanded.FV as EtaExpanded
import qualified EtaReduced.FV as EtaReduced
import qualified Set.FV as Set
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

funMap :: HashMap String (Term -> [Var])
funMap = HashMap.fromList funList

funList :: [(String, Term -> [Var])]
funList = mconcat
  [ NoUnion.funs
  , NoUnionForce.funs
  , NoUnionLazy.funs
  , NoUnionStrictTuple.funs
  , EtaExpanded.funs
  , EtaReduced.funs
  , Set.funs
  , Generic.funs
  ]

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ " <function> <test>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["list-testcases"] -> putStrLn $ unwords $ HashMap.keys testMap
    ["list-functions"] -> putStrLn $ unwords $ map fst funList
    [funName, testName] ->
      case (HashMap.lookup funName funMap, HashMap.lookup testName testMap) of
        (Just f, Just t) -> print $ sum $ f t
        _ -> usage
    _ -> usage
