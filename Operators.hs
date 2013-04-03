module Operators where

import qualified Prelude as P
import SymbolicPrelude
import CalculusTypes

-- | Neat notation for arguments.
arg = Var . Argument

-- | Neat notation for the return value.
return = Var Return

lit = Literal

-- Definitions of the usual equality / ordering relations.
type OrdCond = Expr -> Expr -> Condition

-- | Greater than
(>) :: OrdCond
(>) = GT

-- | Greater than or equal
(>=) :: OrdCond
(>=) = GTE

-- | Less than
(<) :: OrdCond
(<) = LT

-- | Less than or equal
(<=) :: OrdCond
(<=) = LTE

-- | Equality
(==) :: OrdCond
(==) = EQ

-- | Inequality
(/=) :: OrdCond
(/=) = NEQ

-- | Forall quantor.
foreach :: String -> (Expr -> Condition) -> Condition
foreach name f = Forall name $ f (Var $ Scoped name)

-- | Exists quantor.
exists :: String -> (Expr -> Condition) -> Condition
exists name f = Exists name $ f (Var $ Scoped name)


-- Definitions of the usual Boolean operations for symbolic conditions.
type BoolOp = Condition -> Condition -> Condition

-- | Tautology
true :: Condition
true = True

-- | Contradiction
false :: Condition
false = False

-- | Conjunction
(&&) :: BoolOp
(&&) = And

-- | Disjunction
(||) :: BoolOp
(||) = Or

-- | Negation
not :: Condition -> Condition
not = Not

-- | Implication
(==>) :: BoolOp
a ==> b = (not a) || b

-- | Bi-implication
(<==>) :: BoolOp
a <==> b = (a ==> b) && (b ==> a)

-- | Converting a Prelude Bool into its symbolic equivalent.
fromBool :: P.Bool -> Condition
fromBool P.True = True
fromBool P.False = False

