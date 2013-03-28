module Simplify where

import qualified Prelude as P
import Prelude (Eq(..), Num(..), Show, String, Int, Integer, otherwise, id, error, undefined, ($), (.))
import Data.Generics (everywhere, everywhere', mkT)
import CalculusTypes

simplify :: Condition -> Condition
simplify = everywhere (mkT simplifyConstCond) . everywhere' (mkT simplifyArithmetic)

-- Rewrite conditions when we can know their results, increasing readability.
simplifyConstCond :: Condition -> Condition
-- simplifyConstCond e = e -- uncomment to disable condition rewriting / FIXME: remove line for production
simplifyConstCond (GT  (Literal a) (Literal b)) = if a P.>  b then True else False
simplifyConstCond (GTE (Literal a) (Literal b)) = if a P.>= b then True else False
simplifyConstCond (LT  (Literal a) (Literal b)) = if a P.<  b then True else False
simplifyConstCond (LTE (Literal a) (Literal b)) = if a P.<= b then True else False
simplifyConstCond (EQ  (Literal a) (Literal b)) = if a P.== b then True else False
simplifyConstCond (NEQ (Literal a) (Literal b)) = if a P./= b then True else False
simplifyConstCond (And False _)     = False
simplifyConstCond (And _     False) = False
simplifyConstCond (And True  e)     = e
simplifyConstCond (And e     True)  = e
simplifyConstCond (Or  True  _)     = True
simplifyConstCond (Or  _     True)  = True
simplifyConstCond (Or  False e)     = e
simplifyConstCond (Or  e     False) = e
simplifyConstCond (Not (Not e)) = e
simplifyConstCond e = e

-- Simplify additions / subtractions to their canonical form.
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
