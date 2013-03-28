data Condition = GT Expr Expr
               | LT, EQ, ....
               | Forall Name Condition
               | Exists Name Condition
               | And Condition Condition
               | Or, Impl, ...
               | Not Condition
               | CTrue
               | CFalse
               
data Expr = Add Expr Expr
          | Sub, Mul, ....
          | EGT, ELT, ...
          | Literal Integer
          | Var Var
          
data Var = Local Int
         | Param Int
         | Stack Int
         | Scoped Name
         | Return
          
type Name = ...

wp :: Program -> Var -> Condition -> Condition

subst :: Expr -> Var -> Condition -> Condition
subst to from = transform subVar
 where subVar (Var v) | v == from = to
       subVar exp = exp
