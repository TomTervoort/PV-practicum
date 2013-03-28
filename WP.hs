module WP where

import qualified Prelude as P
import Prelude (Eq(..), Num(..), Show, String, Int, Integer, otherwise, id, error, undefined, ($), (.))
import Data.Generics (everywhere, mkT)
import CalculusTypes
import ProgramTypes
import Types
import Simplify

infixl 2 //
(//) :: Expr -> Var -> Condition -> Condition
(//) to from = simplify . everywhere (mkT subVar) -- simplify is important here: otherwise "T + 1 - 1" and "T" do not match!
  where
    subVar e@(Var v) | v == from = to
                     | otherwise = e
    subVar e = e

-- Left-to-right composition, the non-monadic (>>) if you wish.
infixr 1 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

stack :: Integer -> Var
stack 0 = Stack (Var T)
stack n = Stack ((Var T) - fromInteger n)

fromStack = Var . stack
boundT n = Var T >= n

-- Gives the weakest precondition of the given program given a postcondition.
wp :: Sequence -> Condition -> Condition
wp s = P.head . wps s

-- TODO: write as a nice fold.
-- Gives the sequence of weakest preconditions generated.
wps :: Sequence -> Condition -> [Condition]
wps (i : ps) q = 
  case i of
    START -> with q' [ Literal (-1) // T ]
    ADD -> binOp (+)
    SUB -> binOp (-)
    MUL -> binOp (*)
    PUSHLITERAL l      -> with q' [ Literal l // stack 0
                                  , Var T + 1 // T       ]
    POP    -> boundT 0 && with q' [ Var T - 1 // T ]
    RETURN -> boundT 0 && with q  -- Deliberately use q instead of q' here, since there is
                                  -- never a SEQ. This may be benecifical when we implement
                                  -- "multiple RETURN", or the program incorrectly does not
                                  -- end with RETURN.
                          [ fromStack 0 // Return ]
    IFTRUE a b -> boundT 0 && ((fromStack 0 `NEQ` 0 && (with (wp a q') [ Var T - 1 // T ])) || (fromStack 0 `EQ` 0 && (with (wp b q') [ Var T - 1 // T ])))
    -- TODO: add missing instrunctions
  : qq
  where qq@(q' : _) = wps ps q
        binOp f = boundT 1
                  && with q' [ Var T - 1 // T
                             , fromStack 1 `f` fromStack 0 // stack 1 ]
        with :: Condition -> [Condition -> Condition] -> Condition
        with x = P.foldl (|>) x
wps [] q = [q]

