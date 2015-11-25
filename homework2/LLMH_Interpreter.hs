module LLMH_Interpreter where 

import LLMH_Lex
import LLMH_Parse
import LLMH_ExpType
import LLMH_ExpSubst
import LLMH_Evaluator
import LLMH_TypeEnvironments
import LLMH_TypeInference


typeToString :: Type -> String

typeToString (TypeVar str) = str
typeToString (TypeConst str) = str
typeToString (Arrow (TypeVar str, t)) =
     str ++ " -> " ++ (typeToString t)
typeToString (Arrow (TypeConst str, t)) =
     str ++ " -> " ++ (typeToString t)
typeToString (Arrow (List t1, t2)) =
     (typeToString (List t1)) ++ " -> " ++ (typeToString t2)
typeToString (Arrow (Myb t1, t2)) =
     (typeToString (Myb t1)) ++ " -> " ++ (typeToString t2)
typeToString (Arrow (t1, t2)) =
     "(" ++ (typeToString t1) ++ ") -> " ++ (typeToString t2)
typeToString (Myb t) = "Maybe " ++ (typeToString t)
typeToString (List t) = "[" ++ (typeToString t) ++ "]"


-- runLMH "file.hs"
--
-- lexes and parses the MH program text in file.hs and
-- performs static analysis including type inference.
-- Then it enters an interpreter loop: the user inputs an
-- LLMH expression, which the computer and evaluates,
-- outputting the type and resulting value.
-- The loop is exited by inputting ":q"
		

runLLMH filename = do
  progtext <- readFile filename
  let lexed  = llmh_lex progtext
      termDecls = llmh_parseProg lexed
      declVars = checkVars termDecls 
      env = (\x -> case lookup x termDecls of
               Just exp -> exp
	       -- The case below should never occur due to static analysis
               Nothing -> error ("Lookup error - undefined variable: " ++ x))
   in if -- this test implements condition 3 in Note 4 
         all (\x -> (all (\y -> elem y declVars) (freevars (env x)))) declVars 
      then let tenv = initialiseTEnv declVars
               as = tvarsTEnv tenv
               (tenv',as') = inferProg declVars tenv env as
	     in do _ <- putStrLn ""
	           _ <- printTypes tenv'
                   runIn tenv' env as'
      else putStrLn "Out-of-scope variables in program."

checkVars [] = []
checkVars ((x,_):trds) =
  let xs = checkVars trds
    in if notElem x xs then (x:xs)
       else error ("Duplicate declaration for variable " ++ x)


inferProg [] tenv _ as = (tenv, as)

inferProg (x:xs) tenv env as =
  let (s',t',as')  = inferType tenv (env x) as
      tenv' = typeSubstTEnv tenv s'
      (Just t) = (lookup x tenv')
      s = mgu t t'
    in inferProg xs (typeSubstTEnv tenv' s) env as'

printTypes [] = putStrLn ""

printTypes ((x,t):tenv) =
  do _ <- putStrLn (x ++ " :: " ++ (typeToString t))
     printTypes tenv

runIn tenv env as = do
     _ <- putStr "LLMH> "
     textuser <- getLine
     if textuser == ":q" then putStrLn "LLMH goodbye!"
       else let lexeduser = llmh_lex textuser
                exp  = llmh_parseExp lexeduser
		(_, t, _) = inferType tenv exp as
	      in do _ <- putStr "Type: "
                    _ <- putStrLn (typeToString t)
	 	    _ <- putStr "Value: "
                    _ <- putStrLn (evalToString env exp)
                    runIn tenv env as

evalToString  env exp =
  case evaluate env exp of
    Num n -> show n
    Boolean b -> show b
    Op ("appl", Jst, exp') -> "Just " ++ (evalToString env exp')
    Nthg -> "Nothing"
    Nil -> "[]"
    Cons (exp1, exp2) -> (evalToString env exp1) ++ ":" ++ (evalToString env exp2)
    _ -> "..."  -- For non-printable values such as functions

