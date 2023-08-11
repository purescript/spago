module Data.Bifunctor.Join where

import Prelude

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Newtype (class Newtype)

-- | Turns a `Bifunctor` into a `Functor` by equating the two type arguments.
newtype Join :: forall k. (k -> k -> Type) -> k -> Type
newtype Join p a = Join (p a a)

derive instance newtypeJoin :: Newtype (Join p a) _

derive newtype instance eqJoin :: Eq (p a a) => Eq (Join p a)

derive newtype instance ordJoin :: Ord (p a a) => Ord (Join p a)

instance showJoin :: Show (p a a) => Show (Join p a) where
  show (Join x) = "(Join " <> show x <> ")"

instance bifunctorJoin :: Bifunctor p => Functor (Join p) where
  map f (Join a) = Join (bimap f f a)

instance biapplyJoin :: Biapply p => Apply (Join p) where
  apply (Join f) (Join a) = Join (f <<*>> a)

instance biapplicativeJoin :: Biapplicative p => Applicative (Join p) where
  pure a = Join (bipure a a)
