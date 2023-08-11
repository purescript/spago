-- | This module defines the `MonadWriter` type class and its instances.

module Control.Monad.Writer.Class where

import Prelude

import Data.Tuple (Tuple(..))

-- | The `MonadTell w` type class represents those monads which support a
-- | monoidal accumulator of type `w`, where `tell` appends a value to the
-- | accumulator.
-- |
-- | An implementation is provided for `WriterT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Law:
-- |
-- | - `do { tell x ; tell y } = tell (x <> y)`
class (Semigroup w, Monad m) <= MonadTell w m | m -> w where
  tell :: w -> m Unit

-- | An extension of the `MonadTell` class that introduces some operations on
-- | the accumulator:
-- |
-- | - `listen` modifies the result to include the changes to the accumulator.
-- | - `pass` applies the returned function to the accumulator.
-- |
-- | An implementation is provided for `WriterT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Laws in addition to the `MonadTell` law:
-- |
-- | - `do { tell x ; tell y } = tell (x <> y)`
-- | - `listen (pure a) = pure (Tuple a mempty)`
-- | - `listen (writer a x) = tell x $> Tuple a x`
class (Monoid w, MonadTell w m) <= MonadWriter w m | m -> w where
  listen :: forall a. m a -> m (Tuple a w)
  pass :: forall a. m (Tuple a (w -> w)) -> m a

-- | Projects a value from modifications made to the accumulator during an
-- | action.
listens :: forall w m a b. MonadWriter w m => (w -> b) -> m a -> m (Tuple a b)
listens f m = do
  Tuple a w <- listen m
  pure $ Tuple a (f w)

-- | Modify the final accumulator value by applying a function.
censor :: forall w m a. MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass do
  a <- m
  pure $ Tuple a f
