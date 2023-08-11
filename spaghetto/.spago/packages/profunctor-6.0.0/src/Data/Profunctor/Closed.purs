module Data.Profunctor.Closed where

import Prelude

import Data.Profunctor (class Profunctor)

-- | The `Closed` class extends the `Profunctor` class to work with functions.
class Profunctor p <= Closed p where
  closed :: forall a b x. p a b -> p (x -> a) (x -> b)

instance closedFunction :: Closed Function where
  closed = (<<<)
