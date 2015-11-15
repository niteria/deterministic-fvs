module Types where

type Var = Int

data Term
  = Var Var
  | App Term [Term]
  | Lam Var Term
