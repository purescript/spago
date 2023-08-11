module Data.Functor.Invariant where

import Control.Semigroupoid ((<<<))
import Data.Functor (class Functor, map)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Monoid.Alternate (Alternate(..))

-- | A type of functor that can be used to adapt the type of a wrapped function
-- | where the parameterised type occurs in both the positive and negative
-- | position, for example, `F (a -> a)`.
-- |
-- | An `Invariant` instance should satisfy the following laws:
-- |
-- | - Identity: `imap id id = id`
-- | - Composition: `imap g1 g2 <<< imap f1 f2 = imap (g1 <<< f1) (f2 <<< g2)`
-- |
class Invariant :: (Type -> Type) -> Constraint
class Invariant f where
  imap :: forall a b. (a -> b) -> (b -> a) -> f a -> f b

instance invariantFn :: Invariant ((->) a) where
  imap = imapF

instance invariantArray :: Invariant Array where
  imap = imapF

instance invariantAdditive :: Invariant Additive where
  imap f _ (Additive x) = Additive (f x)

instance invariantConj :: Invariant Conj where
  imap f _ (Conj x) = Conj (f x)

instance invariantDisj :: Invariant Disj where
  imap f _ (Disj x) = Disj (f x)

instance invariantDual :: Invariant Dual where
  imap f _ (Dual x) = Dual (f x)

instance invariantEndo :: Invariant (Endo Function) where
  imap ab ba (Endo f) = Endo (ab <<< f <<< ba)

instance invariantMultiplicative :: Invariant Multiplicative where
  imap f _ (Multiplicative x) = Multiplicative (f x)

instance invariantAlternate :: Invariant f => Invariant (Alternate f) where
  imap f g (Alternate x) = Alternate (imap f g x)

-- | As all `Functor`s are also trivially `Invariant`, this function can be
-- | used as the `imap` implementation for any types that has an existing
-- | `Functor` instance.
imapF :: forall f a b. Functor f => (a -> b) -> (b -> a) -> f a -> f b
imapF f _ = map f
