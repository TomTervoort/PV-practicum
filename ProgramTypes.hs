
-- | Contains the representation of a Lang0 program.
module ProgramTypes where

import Types

type Sequence = [Instr]

-- | A single program instruction. Note that START 'declares' the number of parameters and should
--   be placed at the start of a program.
data Instr = START Int
           | ADD | SUB | MUL
           | GT  | GTE | LT | LTE | EQ | NEQ
           | PUSHLITERAL Literal
           | POP
           | SETLOCAL Local Literal
           | LOADLOCAL Local
           | STORELOCAL Local
           | LOADPARAM Param
           | STOREPARAM Param
           | IFTRUE Sequence Sequence
           | RETURN
		       | WHILETRUE Sequence
  deriving (Show, Read)
