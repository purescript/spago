module Data.Lens.Prism.Either
  ( _Left
  , _Right
  , module Data.Profunctor.Choice
  ) where

import Data.Either (Either)
import Data.Lens.Prism (Prism)
import Data.Profunctor.Choice (left, right)

-- | Prism for the `Left` constructor of `Either`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = left

-- | Prism for the `Right` constructor of `Either`.
_Right :: forall a b c. Prism (Either c a) (Either c b) a b
_Right = right
