
module LLMH_TypeInference where
import LLMH_ExpType
import LLMH_TypeEnvironments

--------------------------------------------------------------------------

inferType :: TypeEnv -> Exp -> [String] -> (TypeSubst, Type, [String])

-- inferType tenv exp as
--
---- Performs type inference for expression exp in type environment tenv,
---- where as is a list of type variables in use already.
---- Returns a triple (s, t, as') where t is inferred type, s is accompanying 
---- type substitution and as' is all variables used by end of type inference

inferType tenv (Var x) as =
  case lookup x tenv of
    Just t -> ([], t, as)
    Nothing -> error "Type environment lookup error."

inferType tenv (Num n) as = ([], TypeConst "Integer", as)

inferType tenv (Boolean b) as = ([], TypeConst "Bool", as)

inferType tenv (Cond(exp0, exp1, exp2)) as =
  let (s0, t0, as0) = inferType tenv exp0 as
      s0' = mgu t0 (TypeConst "Bool")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s0) s0'
      (s1, t1, as1) = inferType tenv' exp1 as0
      tenv'' = typeSubstTEnv tenv' s1
      (s2, t2, as2) = inferType tenv'' exp2 as1
      s2' = mgu (typeSubst t1 s2) t2
      s = composeSubstList [s0,s0',s1,s2,s2']
    in seq s0' (restrict s (tvarsTEnv tenv), typeSubst t2 s2', as2)

inferType tenv (Op("==", exp1, exp2)) as =
  let (s1, t1, as1) = inferType tenv exp1 as
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2, as2) = inferType tenv' exp2 as1
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in seq s1' (seq s2' (restrict s (tvarsTEnv tenv), TypeConst "Bool", as2))

inferType tenv (Op("<", exp1, exp2)) as =
  let (s1, t1, as1) = inferType tenv exp1 as
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2, as2) = inferType tenv' exp2 as1
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in seq s1' (seq s2' (restrict s (tvarsTEnv tenv), TypeConst "Bool", as2))

inferType tenv (Op("+", exp1, exp2)) as =
  let (s1, t1, as1) = inferType tenv exp1 as
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2, as2) = inferType tenv' exp2 as1
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in seq s1' (seq s2' (restrict s (tvarsTEnv tenv), TypeConst "Integer", as2))

inferType tenv (Op("-", exp1, exp2)) as =
  let (s1, t1, as1) = inferType tenv exp1 as
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2, as2) = inferType tenv' exp2 as1
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in seq s1' (seq s2' (restrict s (tvarsTEnv tenv), TypeConst "Integer", as2))

inferType tenv (Op("appl", exp1, exp2)) as =
  let (s1, t1, as1) = inferType tenv exp1 as
      tenv' = typeSubstTEnv tenv s1
      (s2, t2, as2) = inferType tenv' exp2 as1
      tenv'' = typeSubstTEnv tenv' s2
      a = freshtvar as2
      s3 = mgu (Arrow (t2, TypeVar a)) (typeSubst t1 s2)
      s = composeSubstList [s1,s2,s3]
    in (restrict s (tvarsTEnv tenv), typeSubst (TypeVar a) s3, a:as2)

inferType tenv (Lam (x, exp0)) as =
  let a = freshtvar as
      tenv' = updateTEnv tenv x (TypeVar a)
      (s0, t0, as0) = inferType tenv' exp0 (a:as)
    in (restrict s0 (tvarsTEnv tenv), Arrow (typeSubst (TypeVar a) s0, t0), as0)

inferType tenv (Let(x, exp1, exp2)) as =
  let a = freshtvar as
      tenv' = updateTEnv tenv x (TypeVar a)
      (s1, t1, as1) = inferType tenv' exp1 (a:as)
      s1' = mgu (typeSubst (TypeVar a) s1) t1
      tenv'' = typeSubstTEnv (typeSubstTEnv tenv' s1) s1'
      (s2, t2, as2) = inferType tenv'' exp2 as1
      s = composeSubstList [s1,s1',s2]
    in seq s1' (restrict s (tvarsTEnv tenv), t2, as2)
    
--
-- COMPLETE THE DEFINITION OF inferType BY ADDING CASES FOR:
--
--     Jst  Nthg  MybCase  Nil  Cons  ListCase
--

inferType tenv Jst as =
    let a = freshtvar as
    in  ([], Arrow (TypeVar a,Myb $ TypeVar a), a:as)
    
inferType tenv Nthg as =
    let a = freshtvar as
    in  ([], Myb $ TypeVar a, a:as)

inferType tenv (MybCase (exp0,y,exp1,exp2)) as =
    let (s0, t0, as0) = inferType tenv exp0 as
        tenv' = typeSubstTEnv tenv s0
        a1 = freshtvar as0
        a1' = TypeVar a1
        s0' = mgu t0 (Myb a1')
        t0' = typeSubst t0 s0'
        a1'' = typeSubst a1' s0'
        tenv'' = typeSubstTEnv tenv' s0'
        as0' = a1:as0

     -- Just y
        tenv1 = updateTEnv tenv'' y a1''
        (p1,t1',as1') = inferType tenv1 exp1 as0'

     -- Nothing
        tenv2 = typeSubstTEnv tenv'' p1
        (s2,t2,as2') = inferType tenv2 exp2 as1'
        s2' = mgu t1' t2
        p2 = composeSubstList [p1,s2,s2']
        t2' = typeSubst t2 s2'
        
        s = composeSubstList [s0,s0',p2]
    in  s0' `seq` s2' `seq` (s, t2', as2')

inferType tenv Nil as =
    let a = freshtvar as
    in  ([], List $ TypeVar a, a:as)

inferType tenv (Cons (exp1,exp2)) as =
    let (s1, t1, as1) = inferType tenv exp1 as
        tenv' = typeSubstTEnv tenv s1
        (s2, t2, as2) = inferType tenv' exp2 as1
        s3 = mgu (List $ typeSubst t1 s2) t2
        s = composeSubstList [s1,s2,s3]
    in  s3 `seq` (restrict s (tvarsTEnv tenv), typeSubst t2 s3, as2)


inferType tenv (ListCase (exp0,exp1,y,z,exp2)) as =
    let (s0, t0, as0) = inferType tenv exp0 as
        tenv' = typeSubstTEnv tenv s0
        a1 = freshtvar as0
        a1' = TypeVar a1
        s0' = mgu t0 (List a1')
        t0' = typeSubst t0 s0'
        a1'' = typeSubst a1' s0'
        tenv'' = typeSubstTEnv tenv' s0'
        as0' = a1:as0

     -- []
        (p1,t1',as1') = inferType tenv'' exp1 as0'
        tenv1' = typeSubstTEnv tenv'' p1

     -- y:z
        tenv2 = updateTEnv (updateTEnv tenv1' y a1'') z (List a1'')
        (s2,t2,as2') = inferType tenv2 exp2 as1'
        s2' = mgu t1' t2
        p2 = composeSubstList [p1,s2,s2']
        t2' = typeSubst t2 s2'
        
        s = composeSubstList [s0,s0',p2]
    in  s0' `seq` s2' `seq` (s, t2', as2')

