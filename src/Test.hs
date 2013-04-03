-- Some testing routines and examples.
module Test where

import qualified Prelude as P
import SymbolicPrelude
import ProgramTypes
import WP
import Pretty
import Operators

test1 = ( true
        , return == 130
        , [ START 0
          , PUSHLITERAL 100
          , PUSHLITERAL 30
          , ADD
          , RETURN ])
test2 = (true, (return == 130),           [START 0, PUSHLITERAL 0, IFTRUE [PUSHLITERAL 100] [PUSHLITERAL 200], PUSHLITERAL 30, ADD, RETURN])
test3 = (true, (return == 130),           [START 0, PUSHLITERAL 1, IFTRUE [PUSHLITERAL 100] [PUSHLITERAL 200], PUSHLITERAL 30, ADD, RETURN])
test4 = (true, (return == arg 1), [START 1, PUSHLITERAL 1, RETURN])
test5 = (true, (return == 10),            [START 2, LOADPARAM 1, LOADPARAM 2, LTE, IFTRUE [PUSHLITERAL 10] [PUSHLITERAL 20], RETURN])
test6 = (true, (return == 10),  [START 1, LOADPARAM 0, IFTRUE [PUSHLITERAL 20] [PUSHLITERAL 10], LOADPARAM 0, IFTRUE [PUSHLITERAL 10] [], RETURN])
test7 = (true, (return == 10), [ START 1
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
test8 = (arg 0 == 10,
         return == 10,          [ START 1
                                                  , LOADPARAM 0
                                                  , IFTRUE
                                                     [ LOADPARAM 0 ]
                                                     []
                                                  , RETURN ])
test9 = (true, return == 2, [START 0, SETLOCAL 0 0, PUSHLITERAL 1, WHILETRUE [PUSHLITERAL 2, LOADLOCAL 0, PUSHLITERAL 1, ADD, STORELOCAL 0, LOADLOCAL 0, GT], LOADLOCAL 0, RETURN])

-- | See max4.lang0.
max4Example = 
           (true, 
            (return >= arg 0) && (return >= arg 1) && (return >= arg 2) && (return >= arg 3),
            
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

-- | The floor of the square root function: return(floor(sqrt(arg0))). See sqrt.lang0.
sqrtExample = 
           (arg 0 >= 0,
            ((return * return) <= arg 0) && (((return + 1) * (return + 1)) > arg 0)
                                         && (arg 0 == 0 || (((return - 1) * (return - 1)) < arg 0)),
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
example3 = (true,
            return == arg 0,
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

-- | Tests whether a natural number is even. See isEven.lang0.
isEvenExample = 
           (arg 0 >= 0,
            (return /= 0 && exists "n" (\n -> (2 * n) == arg 0))
            || (return == 0 && exists "n" (\n -> ((2 * n) + 1) == arg 0)),
              [
                START 1,
                PUSHLITERAL 1,
                WHILETRUE [
                    LOADPARAM 0,
                    PUSHLITERAL 0,
                    EQ,
                    IFTRUE [PUSHLITERAL 1, RETURN] [],

                    LOADPARAM 0,
                    PUSHLITERAL 1,
                    EQ,
                    IFTRUE [PUSHLITERAL 0, RETURN] [],

                    LOADPARAM 0,
                    PUSHLITERAL 2,
                    SUB,

                    PUSHLITERAL 1
                ]
            ]
         )

-- FIXME: remove this one.
-- | Minimal test case for whiletrue.
test10 =   (true,
            return == 123,
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

