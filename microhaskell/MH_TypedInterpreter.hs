
import MH_Lex
import MH_Parse
import MH_Typechecker
import MH_Evaluator


toString :: Type -> String

toString (TypeConst str) = str
toString (TypeOp ("->", TypeConst str, t)) =
     str ++ " -> " ++ (toString t)
toString (TypeOp ("->", t1, t2)) =
     "(" ++ (toString t1) ++ ") -> " ++ (toString t2)

-- runMH "file.hs"
--
-- lexes and parses the MH program text in file.hs and
-- performs static analysis including typechecking.
-- Then it enters an interpreter loop: the user inputs an
-- MH expression, which the computer typechecks and evaluates,
-- outputting the type and resulting value.
-- The loop is exited by inputting ":q"


runMH filename = do
  progtext <- readFile filename
  let lexed  = alexScanTokens progtext
      parsed = mh_parseProg lexed
      (typeDecls,termDecls) = unzip parsed
      declVars = checkVars typeDecls termDecls
      tenv = (\y -> case lookup y typeDecls of
               Just t -> t
               Nothing -> error ("No type declaration for variable: " ++ y))
      env = (\x -> case lookup x termDecls of
               Just exp -> exp
               Nothing -> error ("Lookup error - undefined variable: " ++ x))
   in if typechecks declVars tenv env
        then do _ <- putStrLn "Typechecking successful."
                runIn tenv env
      else putStrLn "Type errors in program."

-- checkVars implements conditions 1 and 2 in Note 4
-- I HAVE NOT YET IMPLEMENTED CONDITION 3
-- Note patterns in definition assume arguments are lists of same length

checkVars [] [] = []
checkVars ((x,_):tyds) ((y,_):trds) =
   if (x==y) then
     let xs = checkVars tyds trds
       in if notElem x xs then (x:xs)
          else error ("Duplicate declaration for variable " ++ x)
   else error ("Declaration mismatch on variables " ++ x ++ " and " ++ y)


-- WILL REPLACE THE CLUNKY CODE BELOW WITH A USE OF "all"

typechecks [] _ _ = True
typechecks (x:xs) tenv env = (hastype tenv (env x) (tenv x)) &&
                                 typechecks xs tenv env

runIn tenv env = do
     _ <- putStr "MH> "
     textuser <- getLine
     if textuser == ":q" then putStrLn "MH goodbye!"
       else let lexeduser = alexScanTokens textuser
                exp  = mh_parseExp lexeduser
                posst = typeof tenv exp
             in case posst of
                (Just t) ->
                    do _ <- putStr "Type: "
                       _ <- putStrLn (toString t)
                       _ <- putStr "Value: "
                       _ <- print (evaluate env exp)
                       runIn tenv env
                Nothing ->
                    do _ <- putStrLn "Type error"
                       runIn tenv env

