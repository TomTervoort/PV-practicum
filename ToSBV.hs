{-# LANGUAGE DeriveDataTypeable #-}

module ToSBV where

import Types
import CalculusTypes (Condition, Expr, Var)
import qualified CalculusTypes as C

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List
import Control.Exception

-- syb
import Data.Generics

-- sbv
import Data.SBV

type ParamArray = SArray Int32 Int32

data IllegalVariableException = IllegalVariableException Var deriving (Typeable)
                              | UnkownScopedNameException Name


instance Show IllegalVariableException where
 show (IllegalVariableException v) = "IllegalVariableException: '" ++ show v 
                                      ++ "' is not allowed in a (weakest) precondition."
 show (UnknownScopedNameException n) = "Undefined scoped name: '" ++ n ++ "'."

instance Exception IllegalVariableException

-- | Transforms the intermediate representation of a (weakest) precondition into a Predicate as 
--   understood by SBV.
preCnditionToPredicate :: Condition -> Predicate
preConditionToPredicate cond = forAll ["a"] $ conditionToParamPredicate cond

-- | Transform a condition into a predicate over a parameter array.
preConditionToParamPredicate :: Condition -> ParamArray -> SBool
preConditionToParamPredicate cond params = c2p [] cond
 where c2p scoped cond = let e2i = expressionToSInteger scoped params in
        case cond of
         C.Forall n c -> forAll  [n] $ \v -> c2p (insert (n, v) scoped) c
         C.Exists n c -> forSome [n] $ \v -> c2p (insert (n, v) scoped) c
         C.True       -> literal True
         C.False      -> literal False
         C.Not c      -> bnot $ c2p scoped c
         C.And a b    -> c2p scoped a &&& c2p scoped b
         C.Or  a b    -> c2p scoped a ||| c2p scoped b
         C.EQ  a b    -> e2i a .== e2i b
         C.NEQ a b    -> e2i a ./= e2i b
         C.GT  a b    -> e2i a .>  e2i b
         C.GTE a b    -> e2i a .>= e2i b
         C.LT  a b    -> e2i a .<  e2i b
         C.LTE a b    -> e2i a .<= e2i b
       
-- | Converts an expression to a symbolic integer based on the parameters and the current 
--   environment of scoped variables.
expressionToSInteger :: ParamArray -> [(Name, SInt32)] -> Expr -> SInt32
expressionToSInteger scoped params = e2i
 where e2i expr =
        case expr of
         C.Add a b            -> e2i a + e2i b
         C.Sub a b            -> e2i a - e2i b
         C.Mul a b            -> e2i a * e2i b
         C.Literal i          -> literal $ fromIntegral i
         C.Var (C.Argument p) -> readArray params $ fromIntegral p
         C.Var (C.Scoped n)   -> fromMaybe (throw $ UnkownScopedNameException n)
                                  $ lookup n scoped
         C.Var var            -> throw $ IllegalVariableException var
