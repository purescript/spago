module Data.Functor.Flip where

import Prelude

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Functor.Contravariant (class Contravariant)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, lcmap)

-- | Flips the order of the type arguments of a `Bifunctor`.
newtype Flip :: forall k1 k2. (k1 -> k2 -> Type) -> k2 -> k1 -> Type
newtype Flip p a b = Flip (p b a)

derive instance newtypeFlip :: Newtype (Flip p a b) _

derive newtype instance eqFlip :: Eq (p b a) => Eq (Flip p a b)

derive newtype instance ordFlip :: Ord (p b a) => Ord (Flip p a b)

instance showFlip :: Show (p a b) => Show (Flip p b a) where
  show (Flip x) = "(Flip " <> show x <> ")"

instance functorFlip :: Bifunctor p => Functor (Flip p a) where
  map f (Flip a) = Flip (lmap f a)

instance bifunctorFlip :: Bifunctor p => Bifunctor (Flip p) where
  bimap f g (Flip a) = Flip (bimap g f a)

instance biapplyFlip :: Biapply p => Biapply (Flip p) where
  biapply (Flip fg) (Flip xy) = Flip (fg <<*>> xy)

instance biapplicativeFlip :: Biapplicative p => Biapplicative (Flip p) where
  bipure a b = Flip (bipure b a)

instance contravariantFlip :: Profunctor p => Contravariant (Flip p b) where
  cmap f (Flip a) = Flip (lcmap f a)

instance semigroupoidFlip :: Semigroupoid p => Semigroupoid (Flip p) where
  compose (Flip a) (Flip b) = Flip $ compose b a

instance categoryFlip :: Category p => Category (Flip p) where
  identity = Flip identity
