
{module LLMH_Lex where }

%wrapper "basic"


$digit		= [0-9]
$lower          = [a-z]
$upper   	= [A-Z]
$symb		= [\!\@\#\$\%\^\&\-\+\=\/\<\>\~\:\;\.\?\/\\\~]

:-

   $white+                              	;   -- whitespace
   \-[\-]+([^$symb].*)?				;   -- comments
   if | then | else | let | in | case | of      {\s -> KEY s}
   \=\= | \< | \+ | \- | \= | \-\>              {\s -> OP s}
   \\                                           {\_ -> OP "lam"}
   \( | \) | \; 				{\s -> PUNC s}
   True | False					{\s -> BOOLEAN (read s)}
   $digit+					{\s -> NUM (read s)}
   Just | Nothing | \[\] | \:                   {\s -> CONSTR s}
   [$lower][$lower $upper $digit]*[\']*   	{\s -> VAR s}
   $symb+					{\_ -> UNKNOWNSYM}


{

-- The Token type - each action above has type String -> Token

llmh_lex = alexScanTokens

data Token =
        KEY String | 
	OP String | 
	PUNC String | 
        BOOLEAN Bool |
	NUM Integer |
        CONSTR String |		
        VAR String |		
	UNKNOWNSYM
	deriving Show
}

