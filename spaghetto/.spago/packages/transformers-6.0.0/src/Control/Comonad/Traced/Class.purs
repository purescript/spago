-- | This module defines the `ComonadTraced` type class and its instances.

module Control.Comonad.Traced.Class where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Traced.Trans (TracedT(..))
import Data.Tuple (Tuple(..))

-- | The `ComonadTraced` type class represents those monads which support relative (monoidal)
-- | position information via `track`.
-- |
-- | - `track` extracts a value at the specified relative position.
-- |
-- | An implementation is provided for `TracedT`.
-- |
-- | Laws:
-- |
-- | - `track mempty = extract`
-- | - `(track s =<= track t) x = track (s <> t) x`
-- |
-- | For example:
-- |
-- | ```purescript
-- | blur :: forall w. (ComonadTraced (Additive Number) w) -> w Number -> w Number
-- | blur = extend \r -> (track (Additive (-1)) r + track (Additive 1) r) / 2
-- | ```
class Comonad w <= ComonadTraced t w | w -> t where
  track :: forall a. t -> w a -> a

-- | Extracts a value at a relative position which depends on the current value.
tracks :: forall w a t. ComonadTraced t w => (a -> t) -> w a -> a
tracks f w = track (f $ extract w) w

-- | Get the current position.
listen :: forall w a t. Functor w => TracedT t w a -> TracedT t w (Tuple a t)
listen (TracedT tr) = TracedT ((\f t -> Tuple (f t) t) <$> tr)

-- | Get a value which depends on the current position.
listens :: forall w a t b. Functor w => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)
listens f (TracedT tr) = TracedT ((\g t -> Tuple (g t) (f t)) <$> tr)

-- | Apply a function to the current position.
censor :: forall w a t. Functor w => (t -> t) -> TracedT t w a -> TracedT t w a
censor f (TracedT tr) = TracedT ((f >>> _) <$> tr)

instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w) where
  track t (TracedT tr) = extract tr t
