-- | Re-exports a very small subset of the Prelude, that does not interact with
-- any symbolic operations we defined.
module SymbolicPrelude (module Prelude) where

import Prelude ( Num(..)
               , Show
               , Read
               , String
               , Int
               , Integer
               , otherwise
               , id
               , error
               , undefined
               , ($)
               , (.)
               , (++)
               , map
               , zip
               , length
               , max
               , min
               , replicate
               , mapM_
               , putStrLn
               )
