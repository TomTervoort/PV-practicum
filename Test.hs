-- Some testing routines, including hacked "pretty" printing for some data types.
module Test where

import Prelude hiding (True, False, GT, LT, EQ)
import CalculusTypes hiding (EQ, NEQ, GT, GTE, LT, LTE)
import qualified CalculusTypes as C
import ProgramTypes
import WP
import Pretty

arg = Var . Argument

test1 = (True, (Var Return `C.EQ` 130),           [START 0, PUSHLITERAL 100, PUSHLITERAL 30, ADD, RETURN])
test2 = (True, (Var Return `C.EQ` 130),           [START 0, PUSHLITERAL 0, IFTRUE [PUSHLITERAL 100] [PUSHLITERAL 200], PUSHLITERAL 30, ADD, RETURN])
test3 = (True, (Var Return `C.EQ` 130),           [START 0, PUSHLITERAL 1, IFTRUE [PUSHLITERAL 100] [PUSHLITERAL 200], PUSHLITERAL 30, ADD, RETURN])
test4 = (True, (Var Return `C.EQ` arg 1), [START 1, PUSHLITERAL 1, RETURN])
test5 = (True, (Var Return `C.EQ` 10),            [START 2, LOADPARAM 1, LOADPARAM 2, LTE, IFTRUE [PUSHLITERAL 10] [PUSHLITERAL 20], RETURN])
test6 = (True, (C.EQ (Var Return) (Literal 10)),  [START 1, LOADPARAM 0, IFTRUE [PUSHLITERAL 20] [PUSHLITERAL 10], LOADPARAM 0, IFTRUE [PUSHLITERAL 10] [], RETURN])
test7 = (True, (C.EQ (Var Return) (Literal 10)),  [ START 1
                                                  , LOADPARAM 0
                                                  , IFTRUE 
                                                      [ SETLOCAL 1 10 ]
                                                      []
                                                  , LOADPARAM 0
                                                  , IFTRUE
                                                      []
                                                      [ SETLOCAL 1 10 ]
                                                  , LOADLOCAL 1
                                                  , IFTRUE
                                                      [ PUSHLITERAL 10 ]
                                                      []
                                                  , RETURN ])
test8 = (Var (Argument 0) `C.EQ` (Literal 10),
         C.EQ (Var Return) (Literal 10),          [ START 1
                                                  , LOADPARAM 0
                                                  , IFTRUE
                                                     [ LOADPARAM 0 ]
                                                     []
                                                  , RETURN ])
test9 = (True, (C.EQ (Var Return) (Literal 2)), [START 0, SETLOCAL 0 0, PUSHLITERAL 1, WHILETRUE [PUSHLITERAL 2, LOADLOCAL 0, PUSHLITERAL 1, ADD, STORELOCAL 0, LOADLOCAL 0, GT], LOADLOCAL 0, RETURN])

example1 = (True, 
            (C.GTE (Var Return) (arg 0)) `And` (C.GTE (Var Return) (arg 1)) `And` (C.GTE (Var Return) (arg 2)) `And` (C.GTE (Var Return) (arg 3)),
            
            [	START 4,
                LOADPARAM 0,
                LOADPARAM 1,
                LOADPARAM 2,
                LOADPARAM 3,
                
                STORELOCAL 0,
                
                STORELOCAL 1,
                LOADLOCAL 0,
                LOADLOCAL 1,
                LT,
                IFTRUE [LOADLOCAL 1, STORELOCAL 0] [],
                
                STORELOCAL 1,
                LOADLOCAL 0,
                LOADLOCAL 1,
                LT,
                IFTRUE [LOADLOCAL 1, STORELOCAL 0] [],
                
                STORELOCAL 1,
                LOADLOCAL 0,
                LOADLOCAL 1,
                LT,
                IFTRUE [LOADLOCAL 1, STORELOCAL 0] [],
                
                LOADLOCAL 0,
                
                RETURN
            ]
           )

-- | The floor of the square root function: return(floor(sqrt(arg0))).
example2 = (C.GTE (arg 0) 0,
            C.LTE (Var Return `Mul` Var Return) (arg 0)
             `And` C.GT ((Var Return `Add` Literal 1) `Mul` (Var Return `Add` Literal 1)) (arg 0),
            [
                START 1,
                SETLOCAL 0 0,
                PUSHLITERAL 1,
                
                WHILETRUE [
                    LOADLOCAL 0,
                    PUSHLITERAL 1,
                    ADD,
                    STORELOCAL 0,
                    
                    LOADLOCAL 0,
                    LOADLOCAL 0,
                    MUL,
                    LOADPARAM 0,
                    LTE
                ],
                
                LOADLOCAL 0,
                PUSHLITERAL 1,
                SUB,
                RETURN
            ]
           )

-- | A rather complicated way of saying return(a0), by incrementing l0 to the proper
-- value step-by-step and then returning it. This is useful to trick the simplifier,
-- it cannot oversimplify this kind of programs.
example3 = (True,
            Var Return `C.EQ` (arg 0),
            [
                START 1,
                SETLOCAL 0 0,
                
                LOADPARAM 0,
                PUSHLITERAL 0,
                NEQ,
                WHILETRUE [
                    LOADPARAM 0,
                    PUSHLITERAL 1,
                    SUB,
                    
                    LOADLOCAL 0,
                    PUSHLITERAL 1,
                    ADD,
                    
                    LOADPARAM 0,
                    PUSHLITERAL 0,
                    NEQ
                ],
                
                LOADPARAM 0,
                RETURN
            ]
           )

-- FIXME: remove this one.
-- | Minimal test case for whiletrue.
test10 =   (True,
            Var Return `C.EQ` 123,
            [
                START 0,
                
                PUSHLITERAL 0,
                WHILETRUE [
                    PUSHLITERAL 55
                ],
                
                PUSHLITERAL 123,
                RETURN
            ]
           )

-- | Debugging tool for programs. Prints the weakest precondition of each instruction
-- in the program.
test (pre, post, prog) = mapM_ (\(x, y) -> putStrLn $ x ++ y) $ zip (map (width 25)
                                $ map ppi prog ++ ["---END---"]) (map ppc $ wps prog post)
  where width :: Int -> String -> String
        width n s = s ++ replicate (max (n - length s) 0) ' '

