{module LMH_Parse where
import LMH_Lex
import LMH_ExpType
}

%name lmh_parseProg Prog
%name lmh_parseExp Exp
%tokentype { Token }
%error { parseError }

%token

--declaration of terminal symbols

         if                {KEY "if"}
         then              {KEY "then"}
         else              {KEY "else"}
         let               {KEY "let"}
         in                {KEY "in"}
         '~'               {OP "=="}
         '<'               {OP "<"}
         '+'               {OP "+"}
         '-'               {OP "-"}
         '='               {OP "="}
         lam               {OP "lam"}
         arrow             {OP "->"}
         '('               {PUNC "("}
         ')'               {PUNC ")"}
         ';'               {PUNC ";"}
         num               {NUM $$}
         boolean           {BOOLEAN $$}
         var               {VAR $$}

-- precedence and associativity declarations, lowest precedence first

%nonassoc '~' '<'
%left '+' '-'

%%

-- the grammar

Prog : {- empty -}             {[]}
     | TermDecl Prog           {$1:$2}

TermDecl : var Args '=' Exp ';' {($1, lamabstract $2 $4)}

Args : {- empty -}         {[]}
     | var Args            {$1:$2}

Exp  : if Exp then Exp else Exp  {Cond ($2, $4, $6)}
     | let var '=' Exp in Exp    {Let ($2, $4, $6)}
     | lam var arrow Exp         {Lam ($2, $4)}
     | Exp0                      {$1}

Exp0 : Exp0 '~' Exp0       {Op ("==", $1, $3)}
     | Exp0 '<' Exp0       {Op ("<", $1, $3)}
     | Exp0 '+' Exp0       {Op ("+", $1, $3)}
     | Exp0 '-' Exp0       {Op ("-", $1, $3)}
     | Exp1                {$1}

Exp1 : Exp1 Exp2           {Op ("appl", $1, $2)}
     | Exp2                {$1}

Exp2 : num                 {Num $1}
     | boolean             {Boolean $1}
     | var                 {Var $1}
     | '(' Exp ')'         {$2}	   

{

parseError :: [Token] -> a
parseError _ = error "LMH parse error"

lamabstract [] exp = exp
lamabstract (v:vs) exp = Lam(v, lamabstract vs exp)

}
