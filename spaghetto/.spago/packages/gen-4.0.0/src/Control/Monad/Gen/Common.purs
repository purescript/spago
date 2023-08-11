module Control.Monad.Gen.Common where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Gen (class MonadGen, chooseFloat, resize, unfoldable)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

-- | Creates a generator that outputs `Either` values, choosing a value from a
-- | `Left` or the `Right` with even probability.
genEither :: forall m a b. MonadGen m => m a -> m b -> m (Either a b)
genEither = genEither' 0.5

-- | Creates a generator that outputs `Either` values, choosing a value from a
-- | `Left` or the `Right` with adjustable bias. As the bias value increases,
-- | the chance of returning a `Left` value rises. A bias ≤ 0.0 will always
-- | return `Right`, a bias ≥ 1.0 will always return `Left`.
genEither' :: forall m a b. MonadGen m => Number -> m a -> m b -> m (Either a b)
genEither' bias genA genB = do
  n <- chooseFloat 0.0 1.0
  if n < bias then Left <$> genA else Right <$> genB

-- | Creates a generator that outputs `Identity` values, choosing a value from
-- | another generator for the inner value.
genIdentity :: forall m a. Functor m => m a -> m (Identity a)
genIdentity = map Identity

-- | Creates a generator that outputs `Maybe` values, choosing a value from
-- | another generator for the inner value. The generator has a 75% chance of
-- | returning a `Just` over a `Nothing`.
genMaybe :: forall m a. MonadGen m => m a -> m (Maybe a)
genMaybe = genMaybe' 0.75

-- | Creates a generator that outputs `Maybe` values, choosing a value from
-- | another generator for the inner value, with an adjustable bias for how
-- | often `Just` is returned vs `Nothing`. A bias ≤ 0.0 will always
-- | return `Nothing`, a bias ≥ 1.0 will always return `Just`.
genMaybe' :: forall m a. MonadGen m => Number -> m a -> m (Maybe a)
genMaybe' bias gen = do
  n <- chooseFloat 0.0 1.0
  if n < bias then Just <$> gen else pure Nothing

-- | Creates a generator that outputs `Tuple` values, choosing values from a
-- | pair of generators for each slot in the tuple.
genTuple :: forall m a b. Apply m => m a -> m b -> m (Tuple a b)
genTuple = lift2 Tuple

-- | Creates a generator that outputs `NonEmpty` values, choosing values from a
-- | generator for each of the items.
-- |
-- | The size of the value will be determined by the current size state
-- | for the generator. To generate a value of a particular size, use the
-- | `resize` function from the `MonadGen` class first.
genNonEmpty
  :: forall m a f
   . MonadRec m
  => MonadGen m
  => Unfoldable f
  => m a
  -> m (NonEmpty f a)
genNonEmpty gen = (:|) <$> gen <*> resize (max 0 <<< (_ - 1)) (unfoldable gen)
