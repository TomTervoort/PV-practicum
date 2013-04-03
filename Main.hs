module Main where

import CalculusTypes (Condition, (==>))
import qualified CalculusTypes as C
import ProgramTypes
import Types
import WP
import Pretty
import ToSBV

import Control.Monad
import System.IO

-- sbv
import Data.SBV.Bridge.Z3 hiding ((==>))

-- | Parses a sequence of program instructions from a string. Currently simply uses the derived implementation of Read.
parseProgram :: String -> Sequence
parseProgram = read

-- | Parses a tuple (precondition, postcondition, program). Currently simply uses the derived implementation of Read.
parseInput :: String -> (Condition, Condition, Sequence)
parseInput = read

-- | Checks whether a predicate is satisfiable. Also returns True when uncertain.
checkSatisfiable :: Condition -> IO Bool
checkSatisfiable cond = do result@(SatResult smt) <- sat $ preConditionToPredicate forSome cond
                           case smt of
                            Unsatisfiable _ -> return False
                            _               -> return True
                         
doProve :: (Condition, Condition, Sequence) -> IO ()
doProve (pre, post, prog) = do let weakestPre = wp prog post
                               
                               putStrLn $ "Given precondition: '" ++ ppc pre ++ "'."
                               -- Warn if precondition is not satisfiable.
                               isSat <- checkSatisfiable pre
                               when (not isSat) $ hPutStrLn stderr "Warning: precondition is not satisfiable.\n"
                               
                               putStrLn $ "Weakest precondition of program: '" ++ ppc weakestPre ++ "'." 
                              
                               -- We need to prove whether given precondition implies wp.
                               let toProve = preConditionToPredicate forAll $ pre ==> weakestPre
                              
                               result <- prove toProve
                               putStrLn $ "\n" ++ show result

main :: IO ()
main = liftM parseInput getContents >>= doProve
          
