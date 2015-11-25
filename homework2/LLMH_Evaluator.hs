
module LLMH_Evaluator where
import LLMH_ExpType
import LLMH_ExpSubst

type Env = String -> Exp


evaluate :: Env -> Exp -> Exp

-- evaluate env e
--
---- performs big step operational semantics on expression exp in
---- term environment env and returns resulting value expression

evaluate env (Var x)  = evaluate env (env x)

evaluate env (Num n)  = (Num n)

evaluate env (Boolean b) = (Boolean b)

evaluate env (Cond(exp0, exp1, exp2)) =
  let val0 = evaluate env exp0
  in case val0 of
    (Boolean True) -> evaluate env exp1
    (Boolean False) -> evaluate env exp2
    _ -> error "Runtime type error"

evaluate env (Let(x, exp1, exp2)) =
  evaluate env (expsubst exp2 x (Let(x, exp1, exp1)))

evaluate env (Op("==", exp1, exp2)) =
  let val1 = evaluate env exp1
      val2 = evaluate env exp2
  in case (val1, val2) of
    (Num m, Num n) -> Boolean (m==n)
    _ -> error "Runtime type error"

evaluate env (Op("<", exp1, exp2)) =
  let val1 = evaluate env exp1
      val2 = evaluate env exp2
  in case (val1, val2) of
    (Num m, Num n) -> Boolean (m<n)
    _ -> error "Runtime type error"

evaluate env (Op("+", exp1, exp2)) =
  let val1 = evaluate env exp1
      val2 = evaluate env exp2
  in case (val1, val2) of
    (Num m, Num n) -> Num (m+n)
    _ -> error "Runtime type error"

evaluate env (Op("-", exp1, exp2)) =
  let val1 = evaluate env exp1
      val2 = evaluate env exp2
  in case (val1, val2) of
    (Num m, Num n) -> Num (m-n)
    _ -> error "Runtime type error"

evaluate env (Op("appl", exp1, exp2)) =
  let val1 = evaluate env exp1
  in case val1 of
    (Lam (x, exp0)) -> evaluate env (expsubst exp0 x exp2)
    _ -> error "Runtime type error"

evaluate env (Lam (x, exp)) = Lam (x, exp)

--
-- COMPLETE THE DEFINITION OF evaluate BY ADDING CASES FOR:
--
--     Jst  Nthg  MybCase  Nil  Cons  ListCase
--
