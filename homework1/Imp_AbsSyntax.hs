module Imp_AbsSyntax where

-- Haskell datatypes representing abstract syntax of expressions and commands
  
-- arithmetic expressions
data AExp =
       Loc String |
       Num Integer |
       AOp (String, AExp, AExp)
       deriving Show

-- boolean expressions
data BExp =
       Boolean Bool |
       BOp (String, AExp, AExp)
       deriving Show

-- commands
data Com =
       Assign (String, AExp) |
       Cond (BExp, Com, Com) |
       Seq (Com, Com) |
       Skip |
       While (BExp, Com)
       deriving Show


