module Control.Monad.Gen.Class where

import Prelude

-- | A class for random generator implementations.
-- |
-- | Instances should provide implementations for the generation functions
-- | that return choices with uniform probability.
-- |
-- | See also `Gen` in `purescript-quickcheck`, which implements this
-- | type class.
class Monad m <= MonadGen m where

  -- | Chooses an integer in the specified (inclusive) range.
  chooseInt :: Int -> Int -> m Int

  -- | Chooses an floating point number in the specified (inclusive) range.
  chooseFloat :: Number -> Number -> m Number

  -- | Chooses a random boolean value.
  chooseBool :: m Boolean

  -- | Modifies the size state for a random generator.
  resize :: forall a. (Size -> Size) -> m a -> m a

  -- | Runs a generator, passing in the current size state.
  sized :: forall a. (Size -> m a) -> m a

type Size = Int
