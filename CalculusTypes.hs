{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}

module CalculusTypes where

import qualified Prelude as P
import SymbolicPrelude
import Data.Generics (Data, Typeable, everywhere, everywhere', mkT)
import Types

class Boolean b where
  true  :: b
  false :: b
  (&&)  :: b -> b -> b
  (||)  :: b -> b -> b
  not   :: b -> b

class (Eq o, Boolean b) => Ord o b where
  (>)  :: o -> o -> b
  (>=) :: o -> o -> b
  (<)  :: o -> o -> b
  (<=) :: o -> o -> b

instance Boolean Condition where
  true  = True
  false = False
  (&&)  = And
  (||)  = Or
  not   = Not

instance Ord Expr Condition where
  (>)  = GT
  (>=) = GTE
  (<)  = LT
  (<=) = LTE

instance Num Expr where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = Literal
  negate e = 0 - e
  abs e = e * signum e
  signum = error "Taking integer sign of symbolic expression."

(==>) :: Boolean b => b -> b -> b
a ==> b = (not a) || b

(<==>) :: Boolean b => b -> b -> b
a <==> b = (a ==> b) && (b ==> a)

fromBool :: Boolean b => P.Bool -> b
fromBool P.True = true
fromBool P.False = false

data Condition = GT Expr Expr | GTE Expr Expr | LT Expr Expr | LTE Expr Expr | EQ Expr Expr | NEQ Expr Expr
               | Forall Name Condition | Exists Name Condition
               | And Condition Condition | Or Condition Condition
               | Not Condition
               | True | False
  deriving (Data, Typeable, Show, Eq, Read)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr -- | ...
          | Literal Literal
          | Var Var
          -- | Bool Condition
  deriving (Data, Typeable, Show, Eq, Read)

data Var = Local Local    -- Local variables.
         | Param Param    -- Program arguments that may have been touched by the program.
         | Argument Param -- Untouched program arguments.
         | Stack Expr     -- Stack location, with an integer expression indicating position relative to the top of the stack.
         | Scoped Name    -- Any scoped / named variable (for quantifiers).
         | Return         -- The return variable.
         | T              -- The stack pointer.
  deriving (Data, Typeable, Show, Eq, Read)

