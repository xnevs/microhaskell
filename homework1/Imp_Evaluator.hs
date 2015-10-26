module Imp_Evaluator where
import Imp_AbsSyntax
import Imp_State

evalAExp :: State -> AExp -> Integer
evalAExp state e = case e of
    Loc s           -> valof state s
    Num n           -> n
    AOp ("+",e1,e2) -> (evalAExp state e1) + (evalAExp state e2)
    AOp ("-",e1,e2) -> (evalAExp state e1) - (evalAExp state e2)
    AOp ("*",e1,e2) -> (evalAExp state e1) * (evalAExp state e2)

evalBExp :: State -> BExp -> Bool
evalBExp state e = case e of
    Boolean b          -> b
    BOp ("==", e1, e2) -> (evalAExp state e1) == (evalAExp state e2)
    BOp ( "<", e1, e2) -> (evalAExp state e1) < (evalAExp state e2)

evalCom :: State -> Com -> State
evalCom state c = case c of
    Assign (loc, e)     -> update state loc $ evalAExp state e
    Cond (cond, c1, c2) -> if evalBExp state cond
                               then evalCom state c1
                               else evalCom state c2
    Seq (c1, c2)        -> evalCom (evalCom state c1) c2
    Skip                -> state
    While (cond, c)     -> if evalBExp state cond
                               then evalCom (evalCom state c) while
                               else state
