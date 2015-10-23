{module Imp_Parse where
import Imp_Lex
import Imp_AbsSyntax
}

-- the parser implements a function
--
--           imp_parse :: [Token] -> Com
--
-- that converts a list of tokens to the abstract syntax tree of a command

%name imp_parse Com 
%tokentype { Token }
%error { parseError }

%token

--declaration of terminal symbols

--
-- [[DECLARE YOUR TERMINAL SYMBOLS HERE]]
--

-- precedence and associativity declarations, lowest precedence first

--
-- [[MAKE PRECEDENCE AND ASSOCIATIVITY DECLARATIONS HERE]]
--
  
%%

-- the grammar

--
-- [[DEFINE YOUR GRAMMAR AND ITS ACTIONS HERE]]
--
     
{
  
parseError :: [Token] -> a
parseError _ = error "Imp parse error"

}

