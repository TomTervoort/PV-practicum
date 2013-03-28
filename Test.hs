-- Some testing routines, including hacked "pretty" printing for some data types.
module Test where

import Prelude hiding (True, False, GT, LT, EQ)
import CalculusTypes
import ProgramTypes
import WP

test1 = test True [START, PUSHLITERAL 100, PUSHLITERAL 30, ADD, RETURN] (Var Return `EQ` 130)
test2 = test True [START, PUSHLITERAL 0, IFTRUE [PUSHLITERAL 100] [PUSHLITERAL 200], PUSHLITERAL 30, ADD, RETURN] (Var Return `EQ` 130)
test3 = test True [START, PUSHLITERAL 1, IFTRUE [PUSHLITERAL 100] [PUSHLITERAL 200], PUSHLITERAL 30, ADD, RETURN] (Var Return `EQ` 130)
test4 = test True [START, PUSHLITERAL 1, RETURN] (Var Return `EQ` Var (Param 1))

test pre prog post = mapM_ (\(x, y) -> putStrLn $ x ++ y) $ zip (map (width 25) $ map show prog ++ ["---END---"]) (map ppc $ wps prog post)
  where width :: Int -> String -> String
        width n s = s ++ replicate (max (n - length s) 0) ' '

ppc :: Condition -> String
ppc (GT a b) = "(" ++ ppe a ++ " > " ++ ppe b ++ ")"
ppc (GTE a b) = "(" ++ ppe a ++ " >= " ++ ppe b ++ ")"
ppc (LT a b) = "(" ++ ppe a ++ " < " ++ ppe b ++ ")"
ppc (LTE a b) = "(" ++ ppe a ++ " <= " ++ ppe b ++ ")"
ppc (EQ a b) = "(" ++ ppe a ++ " == " ++ ppe b ++ ")"
ppc (NEQ a b) = "(" ++ ppe a ++ " != " ++ ppe b ++ ")"
ppc (And a b) = "(" ++ ppc a ++ " && " ++ ppc b ++ ")"
ppc (Or a b) = "(" ++ ppc a ++ " || " ++ ppc b ++ ")"
ppc (Not a) = "!" ++ ppc a
ppc True = "TRUE"
ppc False = "FALSE"

ppe :: Expr -> String
ppe (Add a b) = "(" ++ ppe a ++ " + " ++ ppe b ++ ")"
ppe (Sub a b) = "(" ++ ppe a ++ " - " ++ ppe b ++ ")"
ppe (Mul a b) = "(" ++ ppe a ++ " * " ++ ppe b ++ ")"
ppe (Literal l) = show l
ppe (Var a) = ppv a

ppv :: Var -> String
ppv (Local i) = "LOCAL_" ++ show i
ppv (Param i) = "PARAM_" ++ show i
ppv (Scoped n) = "SCOPED_" ++ n
ppv (Stack e) = "STACK[" ++ ppe e ++ "]"
ppv Return = "RETURN"
ppv T = "T"


