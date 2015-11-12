
module LMH_ExpType where

-- Haskell datatypes representing abstract syntax of LMH types and expressions

-- for LMH allow type variables in types
 
data Type =
       TypeVar String |
       TypeConst String |
       Arrow (Type, Type)
  deriving (Show,Eq)
  
data Exp =
       Num Integer |
       Boolean Bool |
       Var String |
       Op (String, Exp, Exp) |
       Cond (Exp, Exp, Exp) |
       Let (String, Exp, Exp) |
       Lam (String, Exp)
       deriving Show



