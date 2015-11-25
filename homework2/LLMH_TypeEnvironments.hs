
module LLMH_TypeEnvironments where
import LLMH_ExpType
import LLMH_Evaluator

--------------------------------------------------------------------------

--
-- Functions manipulating types and type variables
--


typeVars :: Type -> [String]
typeVars (TypeConst _) = []
typeVars (TypeVar a) = [a]
typeVars (Arrow (t, t')) = (typeVars t) ++ (typeVars t')
typeVars (Myb t) = typeVars t
typeVars (List t) = typeVars t


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

typeSubst (Myb t) s = Myb (typeSubst t s)

typeSubst (List t) s = List (typeSubst t s)



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

--
-- Unification
--

-- mgu t1 t2
--
----- Returns most general unifier of t1 and t2 if one exists. Otherwise raises error.

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

mguFind s (List t1) (List t2) = mguFind s t1 t2

mguFind s (Myb t1) (Myb t2) = mguFind s t1 t2

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

