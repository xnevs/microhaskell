
module LLMH_ExpSubst where
import LLMH_ExpType


freevars :: Exp -> [String]

freevars (Var y) = [y]
freevars (Num _) = []
freevars (Boolean _) = []
freevars (Op(op, exp1, exp2)) = (freevars exp1) ++ (freevars exp2)
freevars (Cond(exp0, exp1, exp2)) =
  (freevars exp0) ++ (freevars exp1) ++ (freevars exp2)
freevars (Lam(y, exp0)) = filter (\x -> x/=y) (freevars exp0)
freevars (Let(y, exp1, exp2)) =
  filter (\x -> x/=y) ((freevars exp1) ++ (freevars exp2))
freevars Jst = []
freevars Nthg = []
freevars Nil = []
freevars (Cons (exp1, exp2)) = (freevars exp1) ++ (freevars exp2)
freevars (MybCase (exp0, y, exp1, exp2)) =
  (freevars exp0) ++ (freevars exp2) ++ filter (\x -> x/=y) (freevars exp1)
freevars (ListCase (exp0, exp1, y, z, exp2)) =
  (freevars exp0) ++ (freevars exp1) ++ filter (\x -> (x/=y && x/=z)) (freevars exp2)


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
expsubst (Let(y, exp1, exp2)) x exp =
  if x ==y then Let(y, exp1, exp2)
  else let xs = freevars exp
        in if notElem y xs
	     then Let(y, expsubst exp1 x exp, expsubst exp2 x exp)
	   else let y' = freshen y ((freevars exp1) ++ (freevars exp2) ++ xs)
                 in Let(y', expsubst (expsubst exp1 y (Var y')) x exp,
		            expsubst (expsubst exp2 y (Var y')) x exp )
expsubst (Lam(y, exp0)) x exp = 
  if x==y then Lam(y, exp0)
  else let xs = freevars exp
        in if notElem y xs then Lam(y, expsubst exp0 x exp)
	   else let y' = freshen y ((freevars exp0) ++ xs)
	         in Lam(y', expsubst (expsubst exp0 y (Var y')) x exp)
expsubst Jst x exp = Jst
expsubst Nthg x exp = Nthg
expsubst Nil x exp = Nil
expsubst (Cons (exp1, exp2)) x exp  =  Cons (expsubst exp1 x exp, expsubst exp2 x exp)
expsubst (MybCase (exp0, y, exp1, exp2)) x exp = 
  if x==y then MybCase (expsubst exp0 x exp, y, exp1, expsubst exp2 x exp)
  else let xs = freevars exp
        in if notElem y xs
	     then MybCase (expsubst exp0 x exp, y, expsubst exp1 x exp, expsubst exp2 x exp)
	   else let y' = freshen y ((freevars exp1) ++ xs)
                 in MybCase (expsubst exp0 x exp, 
		             y', expsubst (expsubst exp1 y (Var y')) x exp,
		             expsubst exp2 x exp)
expsubst (ListCase (exp0, exp1, y, z, exp2)) x exp =
  if x==y || x == z then ListCase (expsubst exp0 x exp, expsubst exp1 x exp, y, z, exp2)
  else let xs = freevars exp
           y' = if notElem y xs then y else freshen y ((freevars exp2) ++ xs)
	   z' = if notElem z xs then z else freshen z ([y'] ++ (freevars exp2) ++ xs)
        in ListCase (expsubst exp0 x exp,
	             expsubst exp1 x exp, 
		     y', z', expsubst (expsubst (expsubst exp2 y (Var y')) z (Var z')) x exp)

