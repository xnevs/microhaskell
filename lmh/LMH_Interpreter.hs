module LMH_Interpreter where 

import LMH_Lex
import LMH_Parse
import LMH_ExpType
import LMH_TypeInference
import LMH_Evaluator


toString :: Type -> String

toString (TypeVar str) = str
toString (TypeConst str) = str
toString (Arrow (TypeVar str, t)) =
     str ++ " -> " ++ (toString t)
toString (Arrow (TypeConst str, t)) =
     str ++ " -> " ++ (toString t)
toString (Arrow (t1, t2)) =
     "(" ++ (toString t1) ++ ") -> " ++ (toString t2)

-- runLMH "file.hs"
--
-- lexes and parses the MH program text in file.hs and
-- performs static analysis including type inference.
-- Then it enters an interpreter loop: the user inputs an
-- MH expression, which the computer and evaluates,
-- outputting the type and resulting value.
-- The loop is exited by inputting ":q"
		

runLMH filename = do
  progtext <- readFile filename
  let lexed  = alexScanTokens progtext
      termDecls = lmh_parseProg lexed
      declVars = checkVars termDecls 
      env = (\x -> case lookup x termDecls of
               Just exp -> exp
	       -- The case below should never occur due to static analysis
               Nothing -> error ("Lookup error - undefined variable: " ++ x))
   in if -- this test implements condition 3 in Note 4 
         all (\x -> (all (\y -> elem y declVars) (freevars (env x)))) declVars 
      then let tenv = initialiseTEnv declVars
               tenv' = inferProg declVars tenv env
	     in do _ <- putStrLn ""
	           _ <- printTypes tenv'
                   runIn tenv' env
      else putStrLn "Out-of-scope variables in program."

checkVars [] = []
checkVars ((x,_):trds) =
  let xs = checkVars trds
    in if notElem x xs then (x:xs)
       else error ("Duplicate declaration for variable " ++ x)

inferProg [] tenv _ = tenv

inferProg (x:xs) tenv env =
  let (s',t')  = inferType tenv (env x)
      tenv' = typeSubstTEnv tenv s'
      (Just t) = (lookup x tenv')
      s = mgu t t'
    in inferProg xs (typeSubstTEnv tenv' s) env


printTypes [] = putStrLn ""

printTypes ((x,t):tenv) =
  do _ <- putStrLn (x ++ " :: " ++ (toString t))
     printTypes tenv

runIn tenv env = do
     _ <- putStr "LMH> "
     textuser <- getLine
     if textuser == ":q" then putStrLn "LMH goodbye!"
       else let lexeduser = alexScanTokens textuser
                exp  = lmh_parseExp lexeduser
		(_, t) = inferType tenv exp
	      in do _ <- putStr "Type: "
                    _ <- putStrLn (toString t)
	 	    _ <- putStr "Value: "
                    _ <- print (evaluate env exp)
                    runIn tenv env
