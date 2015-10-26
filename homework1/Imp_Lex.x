
{module Imp_Lex where }

%wrapper "basic"

-- character classes

$digit      = [0-9]
$lower      = [a-z]
$upper      = [A-Z]

:-

-- lexical classes

    $white+                                 ;
    if | then | else | while | do | skip    {\s -> KEY s}
    \=\= | \< | \+ | \- | \* | \:\=         {\s -> OP s}
    \( | \) | \;                            {\s -> PUNC s}
    $digit+                                 {\s -> NUM (read s)}
    True | False                            {\s -> BOOLEAN (read s)}
    [$lower $upper][$lower $upper $digit]*  {\s -> LOC s}


{

-- The Token type - the action for each lexical class has type String -> Token

data Token = KEY String
           | OP String
           | PUNC String
           | NUM Integer
           | BOOLEAN Bool
           | LOC String
    deriving Show

-- The lexer implements a function
--
--        imp_lex :: String -> [Token]
--
-- which converts a string containing Imp program text into a list of tokens

imp_lex = alexScanTokens

}
