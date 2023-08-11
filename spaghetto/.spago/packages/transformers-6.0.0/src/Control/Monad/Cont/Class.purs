-- | This module defines the `MonadCont` type class and its instances.

module Control.Monad.Cont.Class
  ( class MonadCont
  , callCC
  ) where

import Prelude (class Monad)

-- | The `MonadCont` type class represents those monads which support the
-- | `callCC`, or _call-with-current-continuation_ operation.
-- |
-- | This action makes the current continuation available to the caller.
-- |
-- | For example:
-- |
-- | ```purescript
-- | -- setTimeout :: Number -> Effect Unit -> Effect Unit
-- |
-- | delay :: Number -> ContT Unit Effect Unit
-- | delay n = callCC \cont ->
-- |   lift $ setTimeout n (runContT (cont unit) (\_ -> pure unit))
-- | ```
-- | An implementation is provided for `ContT`, and for other monad transformers
-- | defined in this library.
class Monad m <= MonadCont m where
  callCC :: forall a. ((forall b. a -> m b) -> m a) -> m a
