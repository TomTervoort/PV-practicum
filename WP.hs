module WP (wp, wps) where

import qualified Prelude as P
import SymbolicPrelude
import Data.Generics (everywhere, mkT)
import Control.Arrow
import CalculusTypes
import qualified ProgramTypes as I
import Types
import Simplify

infixl 2 //

-- | Substitutes all occurrences of the variable with the expression in the condition.
(//) :: Expr -> Var -> Condition -> Condition
(//) to from = simplify . everywhere (mkT subVar) -- simplify is important here: otherwise "T + 1 - 1" and "T" do not match!
  where
    subVar e@(Var v) | v == from = to
                     | otherwise = e
    subVar e = e

-- | The maximum degree to which a while loop should be unrolled in order to validate it.
boundedVerificationLength :: Int
boundedVerificationLength = 20
    
-- | Gives the weakest precondition of the given program given a postcondition.
wp :: I.Sequence -> Condition -> Condition
wp s = P.head . wps s

-- | Gives the sequence of weakest preconditions generated.
wps ::  I.Sequence -> Condition -> [Condition]
wps (i : ps) q = (: qq) $ 
  case i of
    -- Initialization.
    -- Initialize the stack pointer to -1 and set all Param by their Argument equivalents.
    I.START p -> with q' $ (Literal (-1) // T) : [Var (Argument n) // Param n | n <- [0..p-1]]
    
    -- Arithmetic operators.
    I.ADD -> binArithOp (+)
    I.SUB -> binArithOp (-)
    I.MUL -> binArithOp (*)

    -- Ordering / equality operators.
    I.LT  -> binOrdOp (<)
    I.LTE -> binOrdOp (<=)
    I.GT  -> binOrdOp (>)
    I.GTE -> binOrdOp (>=)
    I.EQ  -> binOrdOp EQ
    I.NEQ -> binOrdOp NEQ

    -- Stack instructions.
    I.PUSHLITERAL l               -> with q' [ Literal l // stack 0
                                             , Var T + 1 // T           ]
    I.POP             -> boundT 0 && with q' [ Var T - 1 // T           ]
    I.SETLOCAL    k x             -> with q' [ Literal x // Local k     ]
    I.LOADLOCAL   k               -> with q' [ Var (Local k) // stack 0
                                             , Var T + 1 // T           ]
    I.STORELOCAL  k   -> boundT 0 && with q' [ Var T - 1 // T
                                             , fromStack 0 // Local k   ]
    I.LOADPARAM   k               -> with q' [ Var (Param k) // stack 0
                                             , Var T + 1 // T           ]
    I.STOREPARAM  k   -> boundT 0 && with q' [ Var T - 1 // T
                                             , fromStack 0 // Param k   ]
    I.RETURN          -> boundT 0 && with q  -- Since RETURN terminates the program, use
                                             -- q here instead of q'. This implements
                                             -- "return from everywhere".
                                             [ fromStack 0 // Return    ]
    -- Branching instructions.
    I.IFTRUE a b -> boundT 0 
                 && (  (fromStack 0 `NEQ` 0 && (with (wp a q') [ Var T - 1 // T ])) 
                    || (fromStack 0 `EQ`  0 && (with (wp b q') [ Var T - 1 // T ])))
    I.WHILETRUE s -> whileInst boundedVerificationLength s
  
  where 
        -- While instruction.
        whileInst 0 _ = boundT 0 && (fromStack 0 `EQ` 0   ==> with q' [ Var T - 1 // T ])
        whileInst n s = boundT 0 && (fromStack 0 `NEQ` 0  ==> with (wp s (whileInst (n-1) s)) [ Var T - 1 // T ])
        
        -- Calculate the weakest preconditions of the continuation of the program.
        -- Keep a sequence of weakest preconditions for printing purposes.
        qq@(q' : _) = wps ps q

        -- Apply the arithmetic operation f to the two topmost values popped from the
        -- stack, then place the result on the stack.
        binArithOp :: (Expr -> Expr -> Expr) -> Condition
        binArithOp f = boundT 1
                       && with q' [ Var T - 1 // T
                                  , withStack f // stack 1 ]

        -- Apply the f :: (Expr -> Expr -> Condition) to the two topmost values popped
        -- from the stack. If True, place 1 on the stack. Otherwise, place 0 on the stack.
        binOrdOp :: (Expr -> Expr -> Condition) -> Condition
        binOrdOp f = boundT 1
                     && (c     ==> with q' [ Var T - 1 // T
                                           , Literal 1 // stack 1 ])
                     && (not c ==> with q' [ Var T - 1 // T
                                           , Literal 0 // stack 1 ])
          where c = withStack f

        -- Apply f to the two topmost values on the stack.
        withStack :: (Expr -> Expr -> a) -> a
        withStack f = fromStack 1 `f` fromStack 0
        
        -- Apply the condition transformations from left to right.
        with :: Condition -> [Condition -> Condition] -> Condition
        with x fs = P.foldr (>>>) id fs $ x
        
        -- Retrieves the stack variable at the given stack index.
        stack :: Integer -> Var
        stack 0 = Stack (Var T)
        stack n = Stack ((Var T) - fromInteger n)

        -- Retrieves a symbolic expression from the stack at the given index.
        fromStack :: Integer -> Expr
        fromStack = Var . stack
        
        -- Prepares a condition asserting a minimal value for the stack pointer.
        boundT :: Expr -> Condition
        boundT n = Var T >= n

-- Base case: an empty program has the postcondition as its weakest precondition.
wps [] q = [q]

