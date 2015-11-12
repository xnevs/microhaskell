
module LMH_TypeInference where
import LMH_ExpType
import LMH_Evaluator

--------------------------------------------------------------------------

--
-- Functions manipulating types and type variables
--

typeVars :: Type -> [String]
typeVars (TypeConst _) = []
typeVars (TypeVar a) = [a]
typeVars (Arrow (t, t')) = (typeVars t) ++ (typeVars t')


freshtvar :: [String] -> String

freshtvar as = findfreshtvar as 0

findfreshtvar as n =
  let a = "a" ++ (show n)
    in if notElem a as then a
       else findfreshtvar as (n+1)


----------------------------------------------------------------------------

-- Parallel type substitutions

type TypeSubst = [(String,Type)]


typeSubst :: Type -> TypeSubst -> Type

typeSubst (TypeVar a) s =
  let mt = lookup a s
    in case mt of
         (Just t) -> t
         Nothing -> (TypeVar a)

typeSubst (TypeConst tc) s = (TypeConst tc)

typeSubst (Arrow (t, t')) s =
  Arrow (typeSubst t s, typeSubst t' s)


domain :: TypeSubst -> [String]

domain s = [a | (a, _) <- s]


restrict :: TypeSubst -> [String] -> TypeSubst

restrict s as = [(a, t) | (a, t) <- s, elem a as]


composeSubst :: TypeSubst -> TypeSubst -> TypeSubst

-- composeSubst s1 s2
--
---- constructs the composite substitution s1 followed by s2

composeSubst s1 s2 =
  [(a, typeSubst (typeSubst (TypeVar a) s1) s2) | a <- domain s1] ++
    [(a, typeSubst (TypeVar a) s2) | a <- domain s2, notElem a (domain s1)]


composeSubstList :: [TypeSubst] -> TypeSubst

-- composeSubstList [s1,..., sn]
--
---- constructs the composite substitution s1 followed by ... followed by sn.

composeSubstList [] = []
composeSubstList (s:ss) = composeSubst s (composeSubstList ss)

---------------------------------------------------------------------------

-- Unification

mgu :: Type -> Type -> TypeSubst

mgu t1 t2 = mguFind [] t1 t2


mguFind s (TypeConst tc1) (TypeConst tc2) | tc1 == tc2   = s

mguFind s (Arrow (t1, t1')) (Arrow (t2, t2')) =
  let s' = mguFind s t1 t2
    in mguFind s' (typeSubst t1' s') (typeSubst t2' s')

mguFind s (TypeVar a1) (TypeVar a2) =
  if a1 == a2 then s else composeSubst s [(a1, (TypeVar a2))]

mguFind s (TypeVar a1) t2 =
  if notElem a1 (typeVars t2) then composeSubst s [(a1, t2)]
  else error "Occurs check failure."

mguFind s t1 (TypeVar a2) =
  if notElem a2 (typeVars t1) then composeSubst s [(a2, t1)]
  else error "Occurs check failure."

mguFind s t1 t2 = error "Unification failure." 


-----------------------------------------------------------------------------

-- Type environments

type TypeEnv = [(String,Type)]

updateTEnv :: TypeEnv -> String -> Type -> TypeEnv

updateTEnv xts x t =
  (x,t) : [(x',t') | (x',t') <- xts, x' /= x]

varsTEnv tenv = [x | (x,_) <- tenv]

tvarsTEnv tenv = concat [typeVars t | (_,t) <- tenv]


typeSubstTEnv :: TypeEnv -> TypeSubst -> TypeEnv

typeSubstTEnv tenv s = 
  [(x, typeSubst t s) | (x,t) <- tenv]


initialiseTEnv vars = countedTEnv vars 0

countedTEnv :: [String] -> Int -> TypeEnv

countedTEnv [] n = []
countedTEnv (x:xs) n = (x, (TypeVar ("a" ++ (show n)))):(countedTEnv xs (n+1))

-----------------------------------------------------------------------------

inferType :: TypeEnv -> Exp -> (TypeSubst, Type)

inferType tenv (Var x)  =
  let mt = lookup x tenv
    in case mt of
      Just t -> ([], t)
      Nothing -> error "Type environment lookup error."

inferType tenv (Num n)  = ([], TypeConst "Integer")

inferType tenv (Boolean b) = ([], TypeConst "Bool")

inferType tenv (Cond(exp0, exp1, exp2)) =
  let (s0, t0) = inferType tenv exp0
      s0' = mgu t0 (TypeConst "Bool")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s0) s0'
      (s1, t1) = inferType tenv' exp1
      tenv'' = typeSubstTEnv tenv' s1
      (s2, t2) = inferType tenv'' exp2
      s2' = mgu (typeSubst t1 s2) t2
      s = composeSubstList [s0,s0',s1,s2,s2']
    in (s, typeSubst t2 s2')

inferType tenv (Op("==", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Bool")

inferType tenv (Op("<", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Bool")

inferType tenv (Op("+", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Integer")

inferType tenv (Op("-", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv (typeSubstTEnv tenv s1) s1'
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Integer")

inferType tenv (Op("appl", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      tenv' = typeSubstTEnv tenv s1
      (s2, t2) = inferType tenv' exp2
      tenv'' = typeSubstTEnv tenv' s2
      as = tvarsTEnv tenv
      a = freshtvar (as ++ (tvarsTEnv tenv''))
      s3 = mgu (Arrow (t2, TypeVar a)) (typeSubst t1 s2)
      s = composeSubstList [s1,s2,s3]
    in (restrict s as, typeSubst (TypeVar a) s3)

inferType tenv (Lam (x, exp0)) =
  let as = tvarsTEnv tenv
      a = freshtvar as
      tenv' = updateTEnv tenv x (TypeVar a)
      (s0, t0) = inferType tenv' exp0
    in (restrict s0 as, Arrow (typeSubst (TypeVar a) s0, t0))

inferType tenv (Let(x, exp1, exp2)) =
  let as = tvarsTEnv tenv
      a = freshtvar as
      tenv' = updateTEnv tenv x (TypeVar a)
      (s1, t1) = inferType tenv' exp1
      s1' = mgu (typeSubst (TypeVar a) s1) t1
      tenv'' = typeSubstTEnv (typeSubstTEnv tenv' s1) s1'
      (s2, t2) = inferType tenv'' exp2
      s = composeSubstList [s1,s1',s2]
    in (restrict s as, t2)
