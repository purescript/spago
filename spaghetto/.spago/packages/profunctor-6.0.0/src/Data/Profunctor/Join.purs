module Data.Profunctor.Join where

import Prelude

import Data.Functor.Invariant (class Invariant)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)

-- | Turns a `Profunctor` into a `Invariant` functor by equating the two type
-- | arguments.
newtype Join :: forall k. (k -> k -> Type) -> k -> Type
newtype Join p a = Join (p a a)

derive instance newtypeJoin :: Newtype (Join p a) _
derive newtype instance eqJoin :: Eq (p a a) => Eq (Join p a)
derive newtype instance ordJoin :: Ord (p a a) => Ord (Join p a)

instance showJoin :: Show (p a a) => Show (Join p a) where
  show (Join x) = "(Join " <> show x <> ")"

instance semigroupJoin :: Semigroupoid p => Semigroup (Join p a) where
  append (Join a) (Join b) = Join (a <<< b)

instance monoidJoin :: Category p => Monoid (Join p a) where
  mempty = Join identity

instance invariantJoin :: Profunctor p => Invariant (Join p) where
  imap f g (Join a) = Join (dimap g f a)
