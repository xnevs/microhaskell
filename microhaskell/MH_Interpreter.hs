
import MH_Lex
import MH_Parse
import MH_Evaluator

-- runMH "file.hs"
--
-- lexes and parses the MH program text in file.hs and then
-- enters an interpreter loop: the user inputs an
-- MH expression, which the computer evaluates, outputting the result
-- The loop is exited by inputting ":q"
		
runMH filename = do
  progtext <- readFile filename
  let lexed  = alexScanTokens progtext
      parsed = mh_parseProg lexed
      (typeDecls,termDecls) = unzip parsed
      env = (\x -> case lookup x termDecls of
               Just exp -> exp
               Nothing -> error ("Lookup error - undefined variable: " ++ x))
   in runIn env
       
runIn env = do
     _ <- putStr "MH> "
     textuser <- getLine
     if textuser == ":q" then putStrLn "MH goodbye!"
       else let lexeduser = alexScanTokens textuser
                parseduser = mh_parseExp lexeduser
             in do _ <- print (evaluate env parseduser)
                   runIn env

