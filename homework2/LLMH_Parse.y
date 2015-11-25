{module LLMH_Parse where
import LLMH_Lex
import LLMH_ExpType
}

%name llmh_parseProg Prog
%name llmh_parseExp Exp
%tokentype { Token }
%error { parseError }

%token

--declaration of terminal symbols

         if                {KEY "if"}
         then              {KEY "then"}
         else              {KEY "else"}
         let               {KEY "let"}
         in                {KEY "in"}
         case              {KEY "case"}
         of                {KEY "of"}
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
         just              {CONSTR "Just"}
         nothing           {CONSTR "Nothing"}
         nil               {CONSTR "[]"}
         ':'               {CONSTR ":"}
         num               {NUM $$}
         boolean           {BOOLEAN $$}
         var               {VAR $$}

-- precedence and associativity declarations, lowest precedence first

%nonassoc else in of arrow
%nonassoc '~' '<'
%right ':'
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
     | Exp '~' Exp               {Op ("==", $1, $3)}
     | Exp '<' Exp               {Op ("<", $1, $3)}
     | Exp '+' Exp               {Op ("+", $1, $3)}
     | Exp '-' Exp               {Op ("-", $1, $3)}
     | Exp ':' Exp               {Cons ($1, $3)}
     | CaseExp                   {$1}
     | Exp1                      {$1}

CaseExp : case Exp of just var arrow Exp ';' nothing arrow Exp {MybCase ($2, $5, $7, $11)}
        | case Exp of nil arrow Exp ';' var ':' var arrow Exp {ListCase ($2, $6, $8, $10, $12)}

Exp1 : Exp1 Exp2           {Op ("appl", $1, $2)}
     | Exp2                {$1}

Exp2 : num                 {Num $1}
     | boolean             {Boolean $1}
     | var                 {Var $1}
     | just                {Jst}
     | nothing             {Nthg}
     | nil                 {Nil}
     | '(' Exp ')'         {$2}	   

{

parseError :: [Token] -> a
parseError _ = error "LLMH parse error"

lamabstract [] exp = exp
lamabstract (v:vs) exp = Lam(v, lamabstract vs exp)

}
