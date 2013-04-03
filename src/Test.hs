-- Some testing routines and examples.
module Test where

import qualified Prelude as P
import SymbolicPrelude
import ProgramTypes
import WP
import Pretty
import Operators

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

-- | Debugging tool for programs. Prints the weakest precondition of each instruction
-- in the program.
test (pre, post, prog) = mapM_ (\(x, y) -> putStrLn $ x ++ y) $ zip (map (width 25)
                                $ map ppi prog ++ ["---END---"]) (map ppc $ wps prog post)
  where width :: Int -> String -> String
        width n s = s ++ replicate (max (n - length s) 0) ' '

