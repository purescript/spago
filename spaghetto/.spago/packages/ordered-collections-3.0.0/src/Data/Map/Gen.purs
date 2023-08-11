module Data.Map.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt, resize, sized, unfoldable)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.List (List)

-- | Generates a `Map` using the specified key and value generators.
genMap
  :: forall m a b
  . MonadRec m
  => MonadGen m
  => Ord a
  => m a
  -> m b
  -> m (Map a b)
genMap genKey genValue = sized \size -> do
  newSize <- chooseInt 0 size
  resize (const newSize) $
    (fromFoldable :: List (Tuple a b) -> Map a b)
      <$> unfoldable (Tuple <$> genKey <*> genValue)
