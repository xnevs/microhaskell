
module MH_Typechecker where
import MH_Parse

type TypeEnv = String -> Type

updatetenv :: TypeEnv -> String -> Type -> TypeEnv

updatetenv tenv x t y =
  if x == y then t else tenv y


hastype :: TypeEnv -> Exp -> Type -> Bool

hastype tenv (Var x) t = (t == tenv x)

hastype tenv (Num n) t  = (t == TypeConst "Integer")

hastype tenv (Boolean b) t = (t == TypeConst "Bool")

hastype tenv (Cond(exp0, exp1, exp2)) t =
  (hastype tenv exp0 (TypeConst "Bool")) &&
      (hastype tenv exp1 t) && (hastype tenv exp2 t)

hastype tenv (Op("==", exp1, exp2)) t =
  hastype tenv exp1 (TypeConst "Integer")
    && hastype tenv exp2 (TypeConst "Integer")
    && t == TypeConst "Bool" 

hastype tenv (Op("<", exp1, exp2)) t =
  hastype tenv exp1 (TypeConst "Integer")
    && hastype tenv exp2 (TypeConst "Integer")
    && t == TypeConst "Bool" 

hastype tenv (Op("+", exp1, exp2)) t =
  hastype tenv exp1 (TypeConst "Integer")
    && hastype tenv exp2 (TypeConst "Integer")
    && t == TypeConst "Integer" 

hastype tenv (Op("-", exp1, exp2)) t =
  hastype tenv exp1 (TypeConst "Integer")
    && hastype tenv exp2 (TypeConst "Integer")
    && t == TypeConst "Integer" 

hastype tenv (Op("appl", exp1, exp2)) t =
  case (typeof tenv exp2) of
    Just t' -> hastype tenv exp1 (TypeOp ("->", t', t))
    Nothing -> error "Static type error"

hastype tenv (Op(",", exp1, exp2)) t =
    case typeof tenv (Op(",", exp1, exp2)) of
        Just t' -> t == t'
        Nothing -> error "Static type error"
    

hastype tenv (Lam (x, exp)) (TypeOp ("->", t1, t2)) =
  hastype (updatetenv tenv x t1) exp t2

hastype tenv (Op (",", exp1, exp2)) (TypeOp (",", t1, t2)) =
  hastype tenv exp1 t1 && hastype tenv exp2 t2

typeof :: TypeEnv -> Exp -> Maybe Type

typeof tenv (Var x)  = Just (tenv x)

typeof tenv (Num n)  = Just (TypeConst "Integer")

typeof tenv (Boolean b) = Just (TypeConst "Bool")

typeof tenv (Cond(exp0, exp1, exp2)) =
  if hastype tenv exp0 (TypeConst "Bool")
    then case typeof tenv exp1 of
           (Just t) -> if (hastype tenv exp2 t) then Just t else Nothing
           Nothing  -> Nothing
    else Nothing

typeof tenv (Op("==", exp1, exp2)) = 
  if hastype tenv exp1 (TypeConst "Integer")
        && hastype tenv exp2 (TypeConst "Integer")
    then Just (TypeConst "Bool")
    else Nothing

typeof tenv (Op("<", exp1, exp2)) =
  if hastype tenv exp1 (TypeConst "Integer")
        && hastype tenv exp2 (TypeConst "Integer")
    then Just (TypeConst "Bool")
    else Nothing

typeof tenv (Op("+", exp1, exp2)) =
  if hastype tenv exp1 (TypeConst "Integer")
        && hastype tenv exp2 (TypeConst "Integer")
    then Just (TypeConst "Integer")
    else Nothing

typeof tenv (Op("-", exp1, exp2)) =
  if hastype tenv exp1 (TypeConst "Integer")
        && hastype tenv exp2 (TypeConst "Integer")
    then Just (TypeConst "Integer")
    else Nothing

typeof tenv (Op("appl", exp1, exp2)) =
  case (typeof tenv exp1) of
    (Just (TypeOp ("->", t1, t2))) ->
       if hastype tenv exp2 t1
         then Just t2
     else Nothing
    _ -> Nothing


typeof tenv (Op(",", exp1, exp2)) =
    case (typeof tenv exp1, typeof tenv exp2) of
        (Just t1, Just t2) -> Just $ TypeOp (",", t1, t2)
        _                  -> Nothing

typeof tenv (Lam (x, exp)) = Nothing

typeof tenv (Op (",", exp1, exp2)) =
  case (typeof tenv exp1, typeof tenv exp2) of
    (Just t1, Just t2) -> Just (TypeOp (",", t1, t2))
    _ -> Nothing
