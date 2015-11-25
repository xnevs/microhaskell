
module LLMH_ExpType where

-- Haskell datatypes representing abstract syntax of LLMH types and expressions

-- for LLMH allow type variables in types
 
data Type =
       TypeVar String |
       TypeConst String |
       Arrow (Type, Type) |
       Myb Type |                -- Myb t represents Maybe t
       List Type                 -- List t represents [t]
  deriving (Show,Eq)

data Exp =
       Num Integer |
       Boolean Bool |
       Var String |
       Op (String, Exp, Exp) |
       Cond (Exp, Exp, Exp) |
       Let (String, Exp, Exp) |
       Lam (String, Exp) |
       Jst | Nthg | Nil | Cons (Exp,Exp) |
       MybCase (Exp, String, Exp, Exp) |
       ListCase (Exp, Exp, String, String, Exp)
       deriving Show



