module Main where

import Prelude
import Prim.TypeError (class Warn, Text)

-- A type class with a Warn constraint that triggers a UserDefinedWarning
class MyClass a where
  myFunction :: a -> a

instance warnedInstance ::
  ( Warn (Text "This is a custom warning that should be censored")
  ) => MyClass Int where
  myFunction = identity

-- Using the instance triggers the warning at compile time
main :: Int
main = myFunction 42
