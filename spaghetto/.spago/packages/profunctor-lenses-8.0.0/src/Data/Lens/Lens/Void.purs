module Data.Lens.Lens.Void where

import Data.Lens.Lens (Lens', lens)
import Data.Void (Void, absurd)
import Prelude (const)

-- | There is everything in `Void`.
devoid :: forall a. Lens' Void a
devoid = lens absurd const
