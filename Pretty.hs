module Pretty (ppc, ppe, ppv, ppi) where

import CalculusTypes as C
import ProgramTypes

-- | Not so pretty printer for conditions.
ppc :: Condition -> String
ppc (C.GT a b) = "(" ++ ppe a ++ " > " ++ ppe b ++ ")"
ppc (C.GTE a b) = "(" ++ ppe a ++ " >= " ++ ppe b ++ ")"
ppc (C.LT a b) = "(" ++ ppe a ++ " < " ++ ppe b ++ ")"
ppc (C.LTE a b) = "(" ++ ppe a ++ " <= " ++ ppe b ++ ")"
ppc (C.EQ a b) = "(" ++ ppe a ++ " == " ++ ppe b ++ ")"
ppc (C.NEQ a b) = "(" ++ ppe a ++ " != " ++ ppe b ++ ")"
ppc (And a b) = "(" ++ ppc a ++ " && " ++ ppc b ++ ")"
ppc (Or a b) = "(" ++ ppc a ++ " || " ++ ppc b ++ ")"
ppc (Not a) = "!" ++ ppc a
ppc C.True = "TRUE"
ppc C.False = "FALSE"

-- | Not so pretty printer for expressions.
ppe :: Expr -> String
ppe (Add a b) = "(" ++ ppe a ++ " + " ++ ppe b ++ ")"
ppe (Sub a b) = "(" ++ ppe a ++ " - " ++ ppe b ++ ")"
ppe (Mul a b) = "(" ++ ppe a ++ " * " ++ ppe b ++ ")"
ppe (Literal l) = show l
ppe (Var a) = ppv a

-- | Not so pretty printer for variables.
ppv :: Var -> String
ppv (Local i) = "LOCAL_" ++ show i
ppv (Param i) = "PARAM_" ++ show i
ppv (Argument i) = "ARGUMENT_" ++ show i
ppv (Scoped n) = "SCOPED_" ++ n
ppv (Stack e) = "STACK[" ++ ppe e ++ "]"
ppv Return = "RETURN"
ppv T = "T"

-- | Not so pretty printer for instructions.
ppi :: Instr -> String
ppi (IFTRUE a b) = "IFTRUE [" ++ show (length a) ++ "] [" ++ show (length b) ++ "]"
ppi (WHILETRUE s) = "WHILETRUE [" ++ show (length s) ++ "]"
ppi i = show i

