
module MH_Evaluator where
import MH_Parse

type Env = String -> Exp


freevars :: Exp -> [String]

freevars (Var y) = [y]
freevars (Num _) = []
freevars (Boolean _) = []
freevars (Op(op, exp1, exp2)) = (freevars exp1) ++ (freevars exp2)
freevars (Cond(exp0, exp1, exp2)) =
  (freevars exp0) ++ (freevars exp1) ++ (freevars exp2)
freevars (Lam(y, exp0)) = filter (\x -> x/=y) (freevars exp0)


freshen :: String -> [String] -> String

freshen x xs =
  if notElem x xs then x
     else freshen (x ++ "'") xs


expsubst :: Exp -> String -> Exp -> Exp

expsubst (Var y) x exp =
  if x==y then exp else Var y
expsubst (Num n) _ _  = (Num n)
expsubst (Boolean b) _ _  = (Boolean b)
expsubst (Op(op, exp1, exp2)) x exp =
  Op(op, expsubst exp1 x exp, expsubst exp2 x exp)
expsubst (Cond(exp0, exp1, exp2)) x exp =
  Cond(expsubst exp0 x exp, expsubst exp1 x exp, expsubst exp2 x exp)
expsubst (Lam(y, exp0)) x exp = 
  if x==y then Lam(y, exp0)
  else let xs = freevars exp
        in if notElem y xs then Lam(y, expsubst exp0 x exp)
	   else let y' = freshen y ((freevars exp0) ++ xs)
	         in Lam(y', expsubst (expsubst exp0 y (Var y')) x exp)


evaluate :: Env -> Exp -> Exp

evaluate env (Var x)  = evaluate env (env x)

evaluate env (Num n)  = (Num n)

evaluate env (Boolean b) = (Boolean b)

evaluate env (Cond(exp0, exp1, exp2)) =
  let val0 = evaluate env exp0
   in case val0 of
        (Boolean True) -> evaluate env exp1
        (Boolean False) -> evaluate env exp2
        _ -> error "Runtime type error"

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



  



