-- | This module defines the store comonad transformer, `StoreT`.

module Control.Comonad.Store.Trans where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, (<<=))

import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype)

-- | The store comonad transformer.
-- |
-- | This comonad transformer extends the context of a value in the base comonad so that the value
-- | depends on a position of type `s`.
-- |
-- | The `ComonadStore` type class describes the operations supported by this comonad.
newtype StoreT s w a = StoreT (Tuple (w (s -> a)) s)

-- | Unwrap a value in the `StoreT` comonad.
runStoreT :: forall s w a. StoreT s w a -> Tuple (w (s -> a)) s
runStoreT (StoreT s) = s

derive instance newtypeStoreT :: Newtype (StoreT s w a) _

instance functorStoreT :: Functor w => Functor (StoreT s w) where
  map f (StoreT (Tuple w s)) = StoreT $ Tuple ((\h -> h >>> f) <$> w) s

instance extendStoreT :: Extend w => Extend (StoreT s w) where
  extend f (StoreT (Tuple w s)) = StoreT $ Tuple ((\w' s' -> f $ StoreT $ Tuple w' s') <<= w) s

instance comonadStoreT :: Comonad w => Comonad (StoreT s w) where
  extract (StoreT (Tuple w s)) = extract w s

instance comonadTransStoreT :: ComonadTrans (StoreT s) where
  lower (StoreT (Tuple w s)) = (_ $ s) <$> w
