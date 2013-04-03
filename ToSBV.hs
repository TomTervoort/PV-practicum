{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

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

data IllegalVariableException = IllegalVariableException Var
                              | UnknownScopedNameException Name
  deriving (Typeable)


instance Show IllegalVariableException where
 show (IllegalVariableException v) = "IllegalVariableException: '" ++ show v 
                                      ++ "' is not allowed in a (weakest) precondition."
 show (UnknownScopedNameException n) = "Undefined scoped name: '" ++ n ++ "'."

instance Exception IllegalVariableException

getArguments :: Condition -> [Int]
getArguments cond = if null ixs then [] else [0..maximum ixs]
 where ixs = [i | C.Argument i <- listify isParam cond]
       isParam (C.Argument _) = True
       isParam _ = False

-- | Transforms the intermediate representation of a (weakest) precondition into a Predicate as 
--   understood by SBV.
preConditionToPredicate :: Condition -> Predicate
preConditionToPredicate cond = makePred labels []
 where labels = ["a" ++ show i | i <- getArguments cond]
       makePred [] ps = preConditionToParamPredicate cond $ reverse ps
       makePred (l:ls) ps = forAll [l] $ \p -> makePred ls (p:ps)

-- | Transform a condition into a predicate over a parameter array.
preConditionToParamPredicate :: Condition -> [SInt32] -> Predicate
preConditionToParamPredicate cond params = c2p [] cond
 where c2p scoped cond = let e2i = expressionToSInteger params scoped in
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
expressionToSInteger :: [SInt32] -> [(Name, SInt32)] -> Expr -> SInt32
expressionToSInteger params scoped = e2i
 where e2i expr =
        case expr of
         C.Add a b            -> e2i a + e2i b
         C.Sub a b            -> e2i a - e2i b
         C.Mul a b            -> e2i a * e2i b
         C.Literal i          -> literal $ fromIntegral i
         C.Var (C.Argument p) -> params !! p
         C.Var (C.Scoped n)   -> fromMaybe (throw $ UnknownScopedNameException n)
                                  $ lookup n scoped
         C.Var var            -> throw $ IllegalVariableException var
