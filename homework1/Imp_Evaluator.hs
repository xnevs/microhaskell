
module Imp_Evaluator where
import Imp_AbsSyntax
import Imp_State

evalAExp :: State -> AExp -> Integer

evalBExp :: State -> BExp -> Bool

evalCom :: State -> Com -> State

--
-- [[DEFINE EVALUATION FUNCTIONS FOR:
--
--     *  ARITHMETIC EXPRESSIONS
--     *  BOOLEAN  EXPRESSIONS
--     *  COMMANDS
--
--   WITH THE TYPES SPECIFIED ABOVE]]
--
