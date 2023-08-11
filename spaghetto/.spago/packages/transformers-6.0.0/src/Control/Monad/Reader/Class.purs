-- | This module defines the `MonadReader` type class and its instances.

module Control.Monad.Reader.Class where

import Prelude

-- | The `MonadAsk` type class represents those monads which support a global
-- | context that can be provided via the `ask` function.
-- |
-- | An implementation is provided for `ReaderT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Law:
-- |
-- | - `do { ask ; ask } = ask`
class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

instance monadAskFun :: MonadAsk r ((->) r) where
  ask = identity

-- | Projects a value from the global context in a `MonadAsk`.
asks :: forall r m a. MonadAsk r m => (r -> a) -> m a
asks f = f <$> ask

-- | An extension of the `MonadAsk` class that introduces a function `local f x`
-- | that allows the value of the local context to be modified for the duration
-- | of the execution of action `x`.
-- |
-- | An implementation is provided for `ReaderT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Laws in addition to the `MonadAsk` law:
-- |
-- | - `local f ask = f <$> ask`
-- | - `local _ (pure a) = pure a`
-- | - `local f (do { a <- x ; y }) = do { a <- local f x ; local f y }`
class MonadAsk r m <= MonadReader r m | m -> r where
  local :: forall a. (r -> r) -> m a -> m a

instance monadReaderFun :: MonadReader r ((->) r) where
  local = (>>>)
