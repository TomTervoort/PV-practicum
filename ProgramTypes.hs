module ProgramTypes where

import Types

type Sequence = [Instr]

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
  deriving (Show)
