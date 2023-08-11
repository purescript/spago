module Control.Monad.ST.Global
  ( Global
  , toEffect
  ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- | This region allows `ST` computations to be converted into `Effect`
-- | computations so they can be run in a global context.
foreign import data Global :: Region

-- | Converts an `ST` computation into an `Effect` computation.
toEffect :: ST Global ~> Effect
toEffect = unsafeCoerce
