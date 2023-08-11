-- | This module defines the `MonadState` type class and its instances.

module Control.Monad.State.Class where

import Prelude (class Monad, Unit, unit)

import Data.Tuple (Tuple(..))

-- | The `MonadState s` type class represents those monads which support a single piece of mutable
-- | state of type `s`.
-- |
-- | - `state f` updates the state using the function `f`.
-- |
-- | An implementation is provided for `StateT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - `do { get ; get } = get`
-- | - `do { put x ; put y } = put y`
-- | - `do { put x ; get } = put x $> x`
-- | - `do { s <- get ; put s } = pure unit`
-- |
class Monad m <= MonadState s m | m -> s where
  state :: forall a. (s -> (Tuple a s)) -> m a

-- | Get the current state.
get :: forall m s. MonadState s m => m s
get = state \s -> Tuple s s

-- | Get a value which depends on the current state.
gets :: forall s m a. MonadState s m => (s -> a) -> m a
gets f = state \s -> Tuple (f s) s

-- | Set the state.
put :: forall m s. MonadState s m => s -> m Unit
put s = state \_ -> Tuple unit s

-- | Modify the state by applying a function to the current state. The returned
-- | value is the new state value.
modify :: forall s m. MonadState s m => (s -> s) -> m s
modify f = state \s -> let s' = f s in Tuple s' s'

modify_ :: forall s m. MonadState s m => (s -> s) -> m Unit
modify_ f = state \s -> Tuple unit (f s)
