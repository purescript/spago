-- | This module defines the `Traced` comonad.

module Control.Comonad.Traced
  ( Traced
  , runTraced
  , traced
  , module Control.Comonad.Traced.Class
  , module Control.Comonad.Traced.Trans
  ) where

import Prelude

import Control.Comonad.Traced.Class (class ComonadTraced, censor, listen, listens, track, tracks)
import Control.Comonad.Traced.Trans (TracedT(..), runTracedT)

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

-- | The `Traced` comonad is a synonym for the `TracedT` comonad transformer, applied
-- | to the `Identity` monad.
type Traced m = TracedT m Identity

-- | Unwrap a value in the `Traced` comonad.
runTraced :: forall m a. Traced m a -> m -> a
runTraced (TracedT t) = unwrap t

-- | Create a value in context in the `Traced` comonad.
traced :: forall m a. (m -> a) -> Traced m a
traced = Identity >>> TracedT
