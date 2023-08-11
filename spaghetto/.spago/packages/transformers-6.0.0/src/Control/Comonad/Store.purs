-- | This module defines the `Store` comonad.

module Control.Comonad.Store
  ( Store
  , runStore
  , store
  , module Control.Comonad.Store.Class
  , module Control.Comonad.Store.Trans
  ) where

import Prelude

import Control.Comonad.Store.Class (class ComonadStore, experiment, peek, peeks, pos, seek, seeks)
import Control.Comonad.Store.Trans (StoreT(..), runStoreT)

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), swap)

-- | The `Store` comonad is a synonym for the `StoreT` comonad transformer, applied
-- | to the `Identity` monad.
type Store s = StoreT s Identity

-- | Unwrap a value in the `Store` comonad.
runStore :: forall s a. Store s a -> Tuple (s -> a) s
runStore (StoreT s) = swap (unwrap <$> swap s)

-- | Create a value in context in the `Store` comonad.
store :: forall s a. (s -> a) -> s -> Store s a
store f x = StoreT $ Tuple (Identity f) x
