-- | A data type and functions for working with ordered pairs.
module Data.Tuple where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Lazy (class Lazy, defer)
import Data.Eq (class Eq1)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.HeytingAlgebra (implies, ff, tt)
import Data.Ord (class Ord1)

-- | A simple product type for wrapping a pair of component values.
data Tuple a b = Tuple a b

-- | Allows `Tuple`s to be rendered as a string with `show` whenever there are
-- | `Show` instances for both component types.
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "(Tuple " <> show a <> " " <> show b <> ")"

-- | Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
-- | there are `Eq` instances for both component types.
derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)

derive instance eq1Tuple :: Eq a => Eq1 (Tuple a)

-- | Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
-- | whenever there are `Ord` instances for both component types. To obtain
-- | the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
-- | `snd`s are `compare`d.
derive instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)

derive instance ord1Tuple :: Ord a => Ord1 (Tuple a)

instance boundedTuple :: (Bounded a, Bounded b) => Bounded (Tuple a b) where
  top = Tuple top top
  bottom = Tuple bottom bottom

instance semigroupoidTuple :: Semigroupoid Tuple where
  compose (Tuple _ c) (Tuple a _) = Tuple a c

-- | The `Semigroup` instance enables use of the associative operator `<>` on
-- | `Tuple`s whenever there are `Semigroup` instances for the component
-- | types. The `<>` operator is applied pairwise, so:
-- | ```purescript
-- | (Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
-- | ```
instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b) where
  append (Tuple a1 b1) (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)

instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b) where
  mempty = Tuple mempty mempty

instance semiringTuple :: (Semiring a, Semiring b) => Semiring (Tuple a b) where
  add (Tuple x1 y1) (Tuple x2 y2) = Tuple (add x1 x2) (add y1 y2)
  one = Tuple one one
  mul (Tuple x1 y1) (Tuple x2 y2) = Tuple (mul x1 x2) (mul y1 y2)
  zero = Tuple zero zero

instance ringTuple :: (Ring a, Ring b) => Ring (Tuple a b) where
  sub (Tuple x1 y1) (Tuple x2 y2) = Tuple (sub x1 x2) (sub y1 y2)

instance commutativeRingTuple :: (CommutativeRing a, CommutativeRing b) => CommutativeRing (Tuple a b)

instance heytingAlgebraTuple :: (HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (Tuple a b) where
  tt = Tuple tt tt
  ff = Tuple ff ff
  implies (Tuple x1 y1) (Tuple x2 y2) = Tuple (x1 `implies` x2) (y1 `implies` y2)
  conj (Tuple x1 y1) (Tuple x2 y2) = Tuple (conj x1 x2) (conj y1 y2)
  disj (Tuple x1 y1) (Tuple x2 y2) = Tuple (disj x1 x2) (disj y1 y2)
  not (Tuple x y) = Tuple (not x) (not y)

instance booleanAlgebraTuple :: (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b)

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Tuple` with the `<$>` operator, applying the function to the second
-- | component, so:
-- | ```purescript
-- | f <$> (Tuple x y) = Tuple x (f y)
-- | ````
derive instance functorTuple :: Functor (Tuple a)

derive instance genericTuple :: Generic (Tuple a b) _

instance invariantTuple :: Invariant (Tuple a) where
  imap = imapF

-- | The `Apply` instance allows functions to transform the contents of a
-- | `Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
-- | for the `fst` component, so:
-- | ```purescript
-- | (Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
-- | ```
instance applyTuple :: (Semigroup a) => Apply (Tuple a) where
  apply (Tuple a1 f) (Tuple a2 x) = Tuple (a1 <> a2) (f x)

instance applicativeTuple :: (Monoid a) => Applicative (Tuple a) where
  pure = Tuple mempty

instance bindTuple :: (Semigroup a) => Bind (Tuple a) where
  bind (Tuple a1 b) f = case f b of
    Tuple a2 c -> Tuple (a1 <> a2) c

instance monadTuple :: (Monoid a) => Monad (Tuple a)

instance extendTuple :: Extend (Tuple a) where
  extend f t@(Tuple a _) = Tuple a (f t)

instance comonadTuple :: Comonad (Tuple a) where
  extract = snd

instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b) where
  defer f = Tuple (defer $ \_ -> fst (f unit)) (defer $ \_ -> snd (f unit))

-- | Returns the first component of a tuple.
fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

-- | Returns the second component of a tuple.
snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

-- | Turn a function that expects a tuple into a function of two arguments.
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry f a b = f (Tuple a b)

-- | Turn a function of two arguments into a function that expects a tuple.
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry f (Tuple a b) = f a b

-- | Exchange the first and second components of a tuple.
swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple a b) = Tuple b a
