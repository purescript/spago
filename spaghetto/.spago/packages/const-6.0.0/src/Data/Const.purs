module Data.Const where

import Prelude

import Data.Eq (class Eq1)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)

-- | The `Const` type constructor, which wraps its first type argument
-- | and ignores its second. That is, `Const a b` is isomorphic to `a`
-- | for any `b`.
-- |
-- | `Const` has some useful instances. For example, the `Applicative`
-- | instance allows us to collect results using a `Monoid` while
-- | ignoring return values.
newtype Const :: forall k. Type -> k -> Type
newtype Const a b = Const a

derive instance newtypeConst :: Newtype (Const a b) _

derive newtype instance eqConst :: Eq a => Eq (Const a b)

derive instance eq1Const :: Eq a => Eq1 (Const a)

derive newtype instance ordConst :: Ord a => Ord (Const a b)

derive instance ord1Const :: Ord a => Ord1 (Const a)

derive newtype instance boundedConst :: Bounded a => Bounded (Const a b)

instance showConst :: Show a => Show (Const a b) where
  show (Const x) = "(Const " <> show x <> ")"

instance semigroupoidConst :: Semigroupoid Const where
  compose _ (Const x) = Const x

derive newtype instance semigroupConst :: Semigroup a => Semigroup (Const a b)

derive newtype instance monoidConst :: Monoid a => Monoid (Const a b)

derive newtype instance semiringConst :: Semiring a => Semiring (Const a b)

derive newtype instance ringConst :: Ring a => Ring (Const a b)

derive newtype instance euclideanRingConst :: EuclideanRing a => EuclideanRing (Const a b)

derive newtype instance commutativeRingConst :: CommutativeRing a => CommutativeRing (Const a b)

derive newtype instance heytingAlgebraConst :: HeytingAlgebra a => HeytingAlgebra (Const a b)

derive newtype instance booleanAlgebraConst :: BooleanAlgebra a => BooleanAlgebra (Const a b)

derive instance functorConst :: Functor (Const a)

instance invariantConst :: Invariant (Const a) where
  imap = imapF

instance applyConst :: Semigroup a => Apply (Const a) where
  apply (Const x) (Const y) = Const (x <> y)

instance applicativeConst :: Monoid a => Applicative (Const a) where
  pure _ = Const mempty
