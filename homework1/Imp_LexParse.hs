module Imp_LexParse where
import Imp_Lex
import Imp_Parse

imp_lexparse = imp_parse . imp_lex
