
{module Imp_Lex where }

%wrapper "basic"

-- character classes

$digit		= [0-9]
$lower          = [a-z]
$upper   	= [A-Z]

:-

-- lexical classes

--
-- [[IMPLEMENT YOUR LEXICAL CLASSES HERE]]
--


{

-- The Token type - the action for each lexical class has type String -> Token

data Token =
        --
        -- [[IMPLEMENT YOUR Token TYPE HERE]]
	--
	deriving Show

-- The lexer implements a function
--
--        imp_lex :: String -> [Token]
--
-- which converts a string containing Imp program text into a list of tokens

imp_lex = alexScanTokens

}
