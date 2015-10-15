
import MH_Lex
import MH_Parse

-- lexParseProg "file.hs"
--
-- lexes the MH program text in file.hs and prints it
-- then parses the result and prints the result of the parse

lexParseProg filename =
  do
    progtext <- readFile filename
    let lexed  = alexScanTokens progtext
     in do
          _ <- putStrLn "Lexing:"     
          _ <- print lexed
  	  _ <- putStrLn "Parsing:"
	  let parsed = mh_parseProg lexed
	   in print parsed


-- lexParseExp <string representing MH expression>
-- e.g., lexParseExp "x + y"
--
-- lexes the MH expression string in file.hs and prints it
-- then parses the result and prints the result of the parse

lexParseExp expstring =
  let lexed  = alexScanTokens expstring
     in do
          _ <- putStrLn "Lexing:"
          _ <- print lexed
	  _ <- putStrLn "Parsing:"
	  let parsed = mh_parseExp lexed
	   in print parsed


