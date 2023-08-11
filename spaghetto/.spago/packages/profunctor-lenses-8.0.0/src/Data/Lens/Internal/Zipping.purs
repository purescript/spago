module Data.Lens.Internal.Zipping where

import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Closed (class Closed)

newtype Zipping a b = Zipping (a -> a -> b)

derive instance newtypeZipping :: Newtype (Zipping a b) _

instance profunctorZipping :: Profunctor Zipping where
  dimap f g (Zipping z) = Zipping \a1 a2 -> g (z (f a1) (f a2))

instance closedZipping :: Closed Zipping where
  closed (Zipping z) = Zipping \f1 f2 x -> z (f1 x) (f2 x)
