
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

freshtvar tvs = findfreshtvar tvs 0

findfreshtvar tvs n =
  let tv = "a" ++ (show n)
    in if notElem tv tvs then tv
       else findfreshtvar tvs (n+1)


----------------------------------------------------------------------------

-- Parallel type substitutions

type TypeSubst = [(String,Type)]


typeSubst :: TypeSubst -> Type -> Type

typeSubst s (TypeVar a) =
  let mt = lookup a s
    in case mt of
         (Just t) -> t
         Nothing -> (TypeVar a)

typeSubst s (TypeConst tc) = (TypeConst tc)

typeSubst s (Arrow (t, t')) =
  Arrow (typeSubst s t, typeSubst s t')



domain :: TypeSubst -> [String]

domain s = [a | (a, _) <- s]


restrict :: TypeSubst -> [String] -> TypeSubst

restrict s as = [(a, t) | (a, t) <- s, elem a as]


composeSubst :: TypeSubst -> TypeSubst -> TypeSubst

-- composeSubst s1 s2
--
---- constructs the composite substitution s1 followed by s2

composeSubst s1 s2 =
  [(a, typeSubst s2 (typeSubst s1 (TypeVar a))) | a <- domain s1] ++
    [(a, typeSubst s2 (TypeVar a)) | a <- domain s2, notElem a (domain s1)]


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
    in mguFind s' (typeSubst s' t1') (typeSubst s' t2')

mguFind s (TypeVar a1) (TypeVar a2) =
  if a1 == a2 then s else composeSubst s [(a1, (TypeVar a2))]

mguFind s (TypeVar a1) t2 =
  if notElem a1 (typeVars t2) then composeSubst s [(a1, t2)]
  else error ("Occurs check failure." ++ (show s) ++ (show (a1, t2)))

mguFind s t1 (TypeVar a2) =
  if notElem a2 (typeVars t1) then composeSubst s [(a2, t1)]
  else error ("Occurs check failure." ++ (show s) ++ (show (a2, t1)))

mguFind s t1 t2 = error ("Unification failure." ++ (show s) ++ (show t1) ++ (show t2))


-----------------------------------------------------------------------------

-- Type environments

type TypeEnv = [(String,Type)]

updateTEnv :: TypeEnv -> String -> Type -> TypeEnv

updateTEnv xts x t =
  (x,t) : [(x',t') | (x',t') <- xts, x' /= x]

varsTEnv tenv = [x | (x,_) <- tenv]

tvarsTEnv tenv = concat [typeVars t | (_,t) <- tenv]


typeSubstTEnv :: TypeSubst -> TypeEnv -> TypeEnv

typeSubstTEnv s tenv = 
  [(x, typeSubst s t) | (x,t) <- tenv]


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
      s0' = mgu (typeSubst s0 t0) (TypeConst "Bool")
      tenv' = typeSubstTEnv s0' (typeSubstTEnv s0 tenv)
      (s1, t1) = inferType tenv' exp1
      tenv'' = typeSubstTEnv s1 tenv'
      (s2, t2) = inferType tenv'' exp2
      s2' = mgu (typeSubst s2 t1) t2
      s = composeSubstList [s0,s0',s1,s2,s2']
    in (s, typeSubst s2' t2)

inferType tenv (Op("==", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv s1' (typeSubstTEnv s1 tenv)
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Bool")

inferType tenv (Op("<", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv s1' (typeSubstTEnv s1 tenv)
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Bool")

inferType tenv (Op("+", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv s1' (typeSubstTEnv s1 tenv)
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Integer")

inferType tenv (Op("-", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      s1' = mgu t1 (TypeConst "Integer")
      tenv' = typeSubstTEnv s1' (typeSubstTEnv s1 tenv)
      (s2, t2) = inferType tenv' exp2
      s2' = mgu t2 (TypeConst "Integer")
      s = composeSubstList [s1,s1',s2,s2']
    in (s, TypeConst "Integer")

inferType tenv (Op("appl", exp1, exp2)) =
  let (s1, t1) = inferType tenv exp1
      tenv' = typeSubstTEnv s1 tenv
      (s2, t2) = inferType tenv' exp2
      tenv'' = typeSubstTEnv s2 tenv'
      as = tvarsTEnv tenv
      a = freshtvar (as ++ (tvarsTEnv tenv''))
      s3 = mgu (Arrow (t2, TypeVar a)) (typeSubst s2 t1)
      s = composeSubstList [s1,s2,s3]
    in (restrict s as, typeSubst s3 (TypeVar a))

inferType tenv (Lam (x, exp0)) =
  let as = tvarsTEnv tenv
      a = freshtvar as
      tenv' = updateTEnv tenv x (TypeVar a)
      (s0, t0) = inferType tenv' exp0
    in (restrict s0 as, Arrow (typeSubst s0 (TypeVar a), t0))

inferType tenv (Let(x, exp1, exp2)) =
  let as = tvarsTEnv tenv
      a = freshtvar as
      tenv' = updateTEnv tenv x (TypeVar a)
      (s1, t1) = inferType tenv' exp1
      s1' = mgu (typeSubst s (TypeVar a)) t1
      tenv'' = typeSubstTEnv s1' (typeSubstTEnv s1 tenv')
      (s2, t2) = inferType tenv'' exp2
      s = composeSubstList [s1,s1',s2]
    in (restrict s as, t2)
