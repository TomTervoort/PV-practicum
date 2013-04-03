{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

-- | Contains functionality for converting our immediate representation of predicates (the 
--   Condition datatype) into an SBV representation.
module ToSBV (IllegalVariableException, preConditionToPredicate)  where

import Types
import CalculusTypes (Condition, Expr, Var)
import qualified CalculusTypes as C

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List
import Data.Maybe
import Control.Exception
import Control.Monad


-- syb
import Data.Generics

-- sbv
import Data.SBV

-- | Thrown whenever a variable occurs within a condition that is not supposed to be present in a 
--   pre- or post-condition.
data IllegalVariableException = IllegalVariableException Var
                              | UnknownScopedNameException Name
  deriving (Typeable)


instance Show IllegalVariableException where
 show (IllegalVariableException v) = "IllegalVariableException: '" ++ show v 
                                      ++ "' is not allowed in any precondition."
                                      ++ case v of
                                           C.Param _ -> " Reason: parameter index out of bounds."
                                           C.T       -> " Did you forget to issue a START instruction?"
                                           _ -> ""
 show (UnknownScopedNameException n) = "Undefined scoped name: '" ++ n ++ "'."

instance Exception IllegalVariableException

-- | Represents an SBV quantifier such as forAll or forSome.
type Quantifier = [String] -> (SInt32 -> Predicate) -> Predicate

-- | Fetches an enumeration of parameters numbers containing as many parameters as used within a
--   condition.
getArguments :: Condition -> [Int]
getArguments cond = if null ixs then [] else [0..maximum ixs]
 where ixs = [i | C.Argument i <- listify isArgument cond]
       isArgument (C.Argument _) = True
       isArgument _              = False

-- | Locates all 'stale variables' within a condition and label them.
getStaleVarLabels :: Condition -> [String]
getStaleVarLabels cond = map labelStaleVar ss
 where ss = [v | v <- listify (not . null . labelStaleVar) cond]

-- | Provide an appropriate label for a stal variable, which may either be a stack reference, 
--   return value, or local variable reference.
labelStaleVar :: Var -> String
labelStaleVar (C.Stack (C.Literal i)) = "stack[" ++ show i ++ "]"
labelStaleVar (C.Return)              = "return"
labelStaleVar (C.Local i)             = "local" ++ show i
labelStaleVar _ = ""

-- | Transforms the intermediate representation of a (weakest) precondition into a Predicate as 
--   understood by SBV, quantifying over stale variable with the given quantifier.
preConditionToPredicate :: Quantifier -> Condition -> Predicate
preConditionToPredicate q cond = makePred argLabels [] staleLabels []
 where argLabels = ["a" ++ show i | i <- getArguments cond]
       staleLabels = getStaleVarLabels cond
       makePred :: [String] -> [SInt32] -> [String] -> [(String, SInt32)] -> Predicate
       makePred [] ps [] vs = preConditionToParamPredicate cond (reverse ps) vs
       makePred [] ps (s:ss) vs = q [s] $ \v -> makePred [] ps ss ((s, v):vs)
       makePred (l:ls) ps ss vs = q [l] $ \p -> makePred ls (p:ps) ss vs

-- | Transform a condition into a predicate over a parameter array.
preConditionToParamPredicate :: Condition -> [SInt32] -> [(String, SInt32)] -> Predicate
preConditionToParamPredicate cond params stale = c2p [] cond
 where c2p scoped cond = let e2i = expressionToSInteger params scoped stale in
        case cond of
         C.Forall n c -> forAll  [n] $ \v -> c2p ((n,v) : scoped) c
         C.Exists n c -> forSome [n] $ \v -> c2p ((n,v) : scoped) c
         C.Not c      -> liftM bnot $ c2p scoped c
         C.And a b    -> liftM2 (&&&) (c2p scoped a) (c2p scoped b)
         C.Or  a b    -> liftM2 (|||) (c2p scoped a) (c2p scoped b)
         _ -> forAll_ $ case cond of
                         C.True       -> literal True
                         C.False      -> literal False
                         C.EQ  a b    -> e2i a .== e2i b
                         C.NEQ a b    -> e2i a ./= e2i b
                         C.GT  a b    -> e2i a .>  e2i b
                         C.GTE a b    -> e2i a .>= e2i b
                         C.LT  a b    -> e2i a .<  e2i b
                         C.LTE a b    -> e2i a .<= e2i b
       
-- | Converts an expression to a symbolic integer based on the parameters and the current 
--   environment of scoped variables.
expressionToSInteger :: [SInt32] -> [(Name, SInt32)] -> [(String, SInt32)] -> Expr -> SInt32
expressionToSInteger params scoped stale = e2i
 where e2i expr =
        case expr of
         C.Add a b            -> e2i a + e2i b
         C.Sub a b            -> e2i a - e2i b
         C.Mul a b            -> e2i a * e2i b
         C.Literal i          -> literal $ fromIntegral i
         C.Var (C.Argument p) -> params !! p
         C.Var (C.Scoped n)   -> fromMaybe (throw $ UnknownScopedNameException n)
                                  $ lookup n scoped
         C.Var var            -> fromMaybe (throw $ IllegalVariableException var)
                                  $ lookup (labelStaleVar var) stale
