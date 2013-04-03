module Simplify where

import qualified Prelude as P
import SymbolicPrelude
import Data.Generics (everywhere, everywhere', mkT)
import CalculusTypes

-- | Simplifies a condition for legibility and stack pointer matching correctness.
simplify :: Condition -> Condition
simplify = everywhere (mkT simplifyBoolean) . everywhere' (mkT simplifyArithmetic)

-- | Rewrite conditions when we can know their results, increasing readability.
simplifyBoolean :: Condition -> Condition
-- Literal value ordering.
simplifyBoolean (GT  (Literal a) (Literal b)) = fromBool $ a P.>  b
simplifyBoolean (GTE (Literal a) (Literal b)) = fromBool $ a P.>= b
simplifyBoolean (LT  (Literal a) (Literal b)) = fromBool $ a P.<  b
simplifyBoolean (LTE (Literal a) (Literal b)) = fromBool $ a P.<= b
simplifyBoolean (EQ  (Literal a) (Literal b)) = fromBool $ a P.== b
simplifyBoolean (NEQ (Literal a) (Literal b)) = fromBool $ a P./= b
-- Simple Boolean logic substitution rules.
simplifyBoolean (And False _)     = False -- False && A <==> False
simplifyBoolean (And _     False) = False -- A && False <==> False
simplifyBoolean (And True  e)     = e     -- True && A  <==> A
simplifyBoolean (And e     True)  = e     -- A && True  <==> A
simplifyBoolean (Or  True  _)     = True  -- True || A  <==> True
simplifyBoolean (Or  _     True)  = True  -- A || True  <==> True
simplifyBoolean (Or  False e)     = e     -- A || False <==> A
simplifyBoolean (Or  e     False) = e     -- False || A <==> A
simplifyBoolean (Not (Not e)) = e         -- ~(~(A))    <==> A
simplifyBoolean (Not True)    = False     -- ~True      <==> False
simplifyBoolean (Not False)   = True      -- ~False     <==> True
-- If we can proof equality we can substitute EQ and NEQ with True and False.
-- We usually cannot proof inequality (in terms of Expr), so don't rewrite that case.
simplifyBoolean e@(EQ  a b) = if a P.== b then True else e
simplifyBoolean   (NEQ a b) = Not (EQ a b)             -- A /= B  <==> ~(A == B)
simplifyBoolean e@(Or  a b) = if a P.== b then a else  -- A || A  <==> A
  case (a, b) of
    (Not a', _) -> if a' P.== b then True else e       -- ~A || A <==> True
    (_, Not b') -> if a P.== b' then True else e       -- A || ~A <==> True
    _           -> e
simplifyBoolean e@(And a b) = if a P.== b then a else  -- A && A  <==> A
  case (a, b) of
    (Not a', _) -> if a' P.== b then False else e      -- ~A && A <==> False
    (_, Not b') -> if a P.== b' then False else e      -- A && ~A <==> False
    _           -> e
-- Don't rewrite any other conditions.
simplifyBoolean e = e

-- | Simplify additions and subtractions to their canonical form.
-- This is nice to create readable output, but essential for matching
-- the stack pointer!
simplifyArithmetic :: Expr -> Expr
simplifyArithmetic = f 0
  where f cum (Add (Literal a) (Literal b)) = Literal $ a + b + cum
        f cum (Add (Literal l) e)           = f (l + cum) e
        f cum (Add e (Literal l))           = f (l + cum) e
        f cum (Sub (Literal a) (Literal b)) = Literal $ a - b + cum
        f cum (Sub (Literal l) e)           =
          case simplifyArithmetic e of
            Literal l'                     -> Literal $ l - l' + cum
            s                              -> Sub (Literal $ l + cum) s
        f cum (Sub e (Literal l))           = f (cum - l) e
        f cum e | cum ==  0 = e
                | cum P.> 0 = Add e $ Literal cum
                | cum P.< 0 = Sub e $ Literal $ negate cum
