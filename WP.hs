module WP where

import qualified Prelude as P
import Prelude (Eq(..), Num(..), Show, String, Int, Integer, otherwise, id, error, undefined, ($), (.))
import Data.Generics (everywhere, mkT)
import Control.Arrow
import CalculusTypes
import qualified ProgramTypes as I
import Types
import Simplify

infixl 2 //
(//) :: Expr -> Var -> Condition -> Condition
(//) to from = simplify . everywhere (mkT subVar) -- simplify is important here: otherwise "T + 1 - 1" and "T" do not match!
  where
    subVar e@(Var v) | v == from = to
                     | otherwise = e
    subVar e = e

stack :: Integer -> Var
stack 0 = Stack (Var T)
stack n = Stack ((Var T) - fromInteger n)

fromStack = Var . stack
boundT n = Var T >= n

-- Gives the weakest precondition of the given program given a postcondition.
wp :: I.Sequence -> Condition -> Condition
wp s = P.head . wps s

-- TODO: write as a nice fold.
-- Gives the sequence of weakest preconditions generated.
wps :: I.Sequence -> Condition -> [Condition]
wps (i : ps) q = 
  case i of
    I.START p -> with q' $ (Literal (-1) // T) : [Var (Argument n) // Param n | n <- [1..p]]
    I.ADD -> binArithOp (+)
    I.SUB -> binArithOp (-)
    I.MUL -> binArithOp (*)
    I.PUSHLITERAL l      -> with q' [ Literal l // stack 0
                                    , Var T + 1 // T       ]
    I.POP    -> boundT 0 && with q' [ Var T - 1 // T ]
    I.RETURN -> boundT 0 && with q  -- Deliberately use q instead of q' here, since there is
                                  -- never a SEQ. This may be benecifical when we implement
                                  -- "multiple RETURN", or the program incorrectly does not
                                  -- end with RETURN.
                            [ fromStack 0 // Return ]
    I.IFTRUE a b -> boundT 0 && ((fromStack 0 `NEQ` 0 && (with (wp a q') [ Var T - 1 // T ])) || (fromStack 0 `EQ` 0 && (with (wp b q') [ Var T - 1 // T ])))
    I.LT  -> binOrdOp LT
    I.LTE -> binOrdOp LTE
    I.GT  -> binOrdOp GT
    I.GTE -> binOrdOp GTE
    I.EQ  -> binOrdOp EQ
    I.NEQ -> binOrdOp NEQ
  : qq
  where -- Calculate the weakest preconditions of the continuation of the program.
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
wps [] q = [q]

