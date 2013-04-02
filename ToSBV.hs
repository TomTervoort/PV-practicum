{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

module ToSBV where

import Types
import CalculusTypes (Condition, Expr, Var)
import qualified CalculusTypes as C

import Data.Array (Array, (!))
import qualified Data.Array as A
import Control.Exception

-- syb
import Data.Generics

-- sbv
import Data.SBV

type ParamArray = SArray Int32 Int32

data IllegalVariableException = IllegalVariableException Var deriving (Typeable)

instance Show IllegalVariableException where
 show (IllegalVariableException v) = "IllegalVariableException: '" ++ show v 
                                      ++ "' is not allowed in a (weakest) precondition."

instance Exception IllegalVariableException

countParams :: Condition -> Int
countParams = gcount (False `mkQ` varIsParam)
 where varIsParam (C.Param _) = True
       varIsParam _ = False

conditionToPredicate :: Condition -> Predicate
conditionToPredicate cond = forAll ["a"] $ conditionToParamPredicate cond

conditionToParamPredicate :: Condition -> ParamArray -> SBool
conditionToParamPredicate cond params = c2p cond
 where e2i = expressionToSInteger params
       c2p cond =
        case cond of
        -- TODO: forall, exists?
         C.True    -> literal True
         C.False   -> literal False
         C.Not c   -> bnot $ c2p c
         C.And a b -> c2p a &&& c2p b
         C.Or  a b -> c2p a ||| c2p b
         C.EQ  a b -> e2i a .== e2i b
         C.NEQ a b -> e2i a ./= e2i b
         C.GT  a b -> e2i a .>  e2i b
         C.GTE a b -> e2i a .>= e2i b
         C.LT  a b -> e2i a .<  e2i b
         C.LTE a b -> e2i a .<= e2i b
       

expressionToSInteger :: ParamArray -> Expr -> SInt32
expressionToSInteger params = e2i
 where e2i expr =
        case expr of
         C.Add a b            -> e2i a + e2i b
         C.Sub a b            -> e2i a - e2i b
         C.Mul a b            -> e2i a * e2i b
         C.Literal i          -> literal $ fromIntegral i
         C.Var (C.Argument p) -> readArray params $ fromIntegral p
         C.Var var            -> throw $ IllegalVariableException var
