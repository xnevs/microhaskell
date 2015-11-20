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

    if      {KEY "if"}
    then    {KEY "then"}
    else    {KEY "else"}
    while   {KEY "while"}
    do      {KEY "do"}
    skip    {KEY "skip"}
    '+'     {OP "+"}
    '-'     {OP "-"}
    '*'     {OP "*"}
    '~'     {OP "=="}
    '<'     {OP "<"}
    assign  {OP ":="}
    '('     {PUNC "("}
    ')'     {PUNC ")"}
    ';'     {PUNC ";"}
    num     {NUM $$}
    boolean {BOOLEAN $$}
    loc     {LOC $$}


-- precedence and associativity declarations, lowest precedence first

%nonassoc then else do
%right ';'
%left '+' '-'
%left '*'
  
%%

-- the grammar

Com : loc assign AExp               {Assign ($1, $3)}
    | if BExp then Com else Com     {Cond ($2, $4, $6)}
    | Com ';' Com                   {Seq ($1, $3)}
    | skip                          {Skip}
    | while BExp do Com             {While ($2, $4)}
    | '(' Com ')'                   {$2}

BExp : boolean                      {Boolean $1}
     | AExp '~' AExp                {BOp ("==", $1, $3)}
     | AExp '<' AExp                {BOp ("<", $1, $3)}

AExp : loc                          {Loc $1}
     | num                          {Num $1}
     | AExp '+' AExp                {AOp ("+", $1, $3)}
     | AExp '-' AExp                {AOp ("-", $1, $3)}
     | AExp '*' AExp                {AOp ("*", $1, $3)}
     
{
  
parseError :: [Token] -> a
parseError _ = error "Imp parse error"

}

