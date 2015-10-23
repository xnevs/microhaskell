module Imp_Interpreter where
import Imp_Lex
import Imp_Parse
import Imp_State
import Imp_Evaluator

-- runImp "file.imp" s 
--
-- lexes and parses the Imp program text in file.imp and then
-- executes it starting in state s
		
runImp filename s = do
    progtext <- readFile filename
    let lexed   = imp_lex progtext
        parsed  = imp_parse lexed
        s'      = evalCom s parsed
      in print s'
