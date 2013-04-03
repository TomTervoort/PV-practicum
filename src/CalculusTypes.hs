{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}

module CalculusTypes where

import qualified Prelude as P
import SymbolicPrelude
import Data.Generics (Data, Typeable)
import Types

-- | Num instance for Expr mostly so we can write "+ 1" without all the datastructure mess.
instance Num Expr where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = Literal
  negate e = 0 - e
  abs e = e * signum e
  signum = error "Taking integer sign of symbolic expression."

-- | Intermediate representation of a boolean predicate. This is used to represent preconditions,
--   postconditions and intermediate forms thereof.
data Condition = GT Expr Expr | GTE Expr Expr | LT Expr Expr | LTE Expr Expr | EQ Expr Expr | NEQ Expr Expr
               | Forall Name Condition | Exists Name Condition
               | And Condition Condition | Or Condition Condition
               | Not Condition
               | True | False
  deriving (Data, Typeable, Show, P.Eq, Read)

-- | An integral expression.
data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr
          | Literal Literal
          | Var Var
  deriving (Data, Typeable, Show, P.Eq, Read)

-- | A variable.
data Var = Local Local    -- Local variables.
         | Param Param    -- Program arguments that may have been touched by the program.
         | Argument Param -- Untouched program arguments.
         | Stack Expr     -- Stack location, with an integer expression indicating position relative to the top of the stack.
         | Scoped Name    -- Any scoped / named variable (for quantifiers).
         | Return         -- The return variable.
         | T              -- The stack pointer.
  deriving (Data, Typeable, Show, P.Eq, Read)

