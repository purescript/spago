-- | This module defines the `ComonadStore` type class and its instances.

module Control.Comonad.Store.Class where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Env.Trans (EnvT)
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced.Trans (TracedT)
import Control.Comonad.Trans.Class (lower)
import Control.Extend (duplicate)
import Data.Tuple (Tuple(..))

-- | The `ComonadStore` type class represents those monads which support local position information via
-- | `pos` and `peek`.
-- |
-- | - `pos` reads the current position.
-- | - `peek` reads the value at the specified position in the specified context.
-- |
-- | An implementation is provided for `StoreT`.
-- |
-- | Laws:
-- |
-- | - `pos (extend _ x) = pos x`
-- | - `peek (pos x) x = extract x`
-- |
-- | For example:
-- |
-- | ```purescript
-- | blur :: forall w. (ComonadStore Number w) -> w Number -> w Number
-- | blur = extend \r -> (peeks (\n -> n - 1) r + peeks (\n -> n + 1) r) / 2)
-- | ```
class Comonad w <= ComonadStore s w | w -> s where
  pos :: forall a. w a -> s
  peek :: forall a. s -> w a -> a

-- | Extract a collection of values from positions which depend on the current position.
experiment :: forall f a w s. ComonadStore s w => Functor f => (s -> f s) -> w a -> f a
experiment f x = flip peek x <$> f (pos x)

-- | Extract a value from a position which depends on the current position.
peeks :: forall s a w. ComonadStore s w => (s -> s) -> w a -> a
peeks f x = peek (f $ pos x) x

-- | Reposition the focus at the specified position.
seek :: forall s a w. ComonadStore s w => s -> w a -> w a
seek s = peek s <<< duplicate

-- | Reposition the focus at the specified position, which depends on the current position.
seeks :: forall s a w. ComonadStore s w => (s -> s) -> w a -> w a
seeks f = peeks f <<< duplicate

instance comonadStoreStoreT :: Comonad w => ComonadStore s (StoreT s w) where
  pos (StoreT (Tuple _ s)) = s
  peek s (StoreT (Tuple f _)) = extract f s

instance comonadStoreEnvT :: ComonadStore s w => ComonadStore s (EnvT e w) where
  pos = pos <<< lower
  peek s = peek s <<< lower

instance comonadStoreTracedT :: (ComonadStore s w, Monoid m) => ComonadStore s (TracedT m w) where
  pos = pos <<< lower
  peek s = peek s <<< lower
