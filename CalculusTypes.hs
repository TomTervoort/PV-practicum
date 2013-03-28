{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}

module CalculusTypes where

import qualified Prelude as P
import Prelude (Eq(..), Num(..), Show, String, Int, Integer, otherwise, id, error, undefined, ($), (.), (++))
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

data Condition = GT Expr Expr | GTE Expr Expr | LT Expr Expr | LTE Expr Expr | EQ Expr Expr | NEQ Expr Expr
               -- | Forall Name Condition | ...
               | And Condition Condition | Or Condition Condition
               | Not Condition
               | True | False
  deriving (Data, Typeable, Show)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr -- | ...
          | Literal Literal
          | Var Var
  deriving (Data, Typeable, Show, Eq)

data Var = Local Local  | Param Param
         | Stack Expr   | Scoped Name
         | Return       | T
  deriving (Data, Typeable, Show, Eq)

