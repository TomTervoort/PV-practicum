{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, NoMonomorphismRestriction #-}

module Main where

import qualified Prelude as P
import Prelude (Eq(..), Num(..), Show, String, Int, Integer, otherwise, id, error, undefined, ($), (.))
import Data.Generics (Data, Typeable, everywhere, everywhere', mkT)

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
 
data Condition = GT Expr Expr | GTE Expr Expr | LT Expr Expr | LTE Expr Expr
               -- | Forall Name Condition | ...
               | And Condition Condition | Or Condition Condition
               | Not Condition
               | True | False
  deriving (Data, Typeable, Show)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr -- | ...
          | Literal Literal
          | Var Var
  deriving (Data, Typeable, Show)

-- Assume we expressions are never equal, since we cannot trivially establish
-- equality on expressions.
instance Eq Expr where
  (==) _ _ = P.False

data Var = Local Local  | Param Param
         | Stack Expr
         | Scoped Name
         | Return       | T
  deriving (Data, Typeable, Show, Eq)

type Literal = Integer
type Local   = Int
type Param   = Int
type Name    = String
type Program = [Instr]

data Instr = ADD
           | SUB
           | MUL
           | PUSHLITERAL Literal
           | POP
           | SETLOCAL Local Literal
           | LOADLOCAL Local
           | STORELOCAL Local
           | LOADPARAM Param
           | STOREPARAM Param
--           | IFTRUE Program Program
           | RETURN

infix 1 //
(//) :: (Data a) => Expr -> Var -> a -> a
(//) to from = everywhere (mkT subVar)
  where
    subVar (Var (Stack e)) = Var . Stack . (to // from) $ e
    subVar e@(Var v) | v == from = to
                     | otherwise = e
    subVar e = e

simplify :: Condition -> Condition
simplify = everywhere' (mkT simplifyArithmetic)

-- Simplify arithmetic expressions a little bit by accumulation.
-- Mostly usefor for eliminating the additions / subtractions on the stack pointer.
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


stack :: Integer -> Var
stack 0 = Stack (Var T)
stack n = Stack ((Var T) - fromInteger n)

-- Some kind of left-to-right composition, the non-monadic (>>) if you wish.
infixl 0 %%
(%%) :: a -> (a -> b) -> b
(%%) x f = f x

--decT :: Condition -> Condition
decT = Var T - 1 // T
incT = Var T + 1 // T

fromStack = Var . stack
assertT n = Var T >= n

wp :: Program -> Condition -> Condition
wp (i : ps) q =
  case i of
    ADD -> binOp (+)
    SUB -> binOp (-)
    MUL -> binOp (*)
    PUSHLITERAL l       -> q' 
                        %% Literal l // stack 0 
                        %% incT
    POP    -> assertT 0 && q'
                        %% decT
    RETURN -> assertT 0 && q  -- Deliberately use q instead of q' here, since there is no SEQ. This may be benecifical when we implement "multiple RETURN".
                        %% fromStack 0 // Return
  where q' = wp ps q
        binOp f = assertT 1 && q'
                            %% decT 
                            %% fromStack 1 `f` fromStack 0 // stack 1
wp [] q = q

