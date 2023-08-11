module Data.Lens.Lens.Tuple
  ( _1
  , _2
  , module Data.Profunctor.Strong
  ) where

import Data.Lens.Lens (Lens)
import Data.Profunctor.Strong (first, second)
import Data.Tuple (Tuple)

-- | Lens for the first component of a `Tuple`.
_1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_1 = first

-- | Lens for the second component of a `Tuple`.
_2 :: forall a b c. Lens (Tuple c a) (Tuple c b) a b
_2 = second
