module Effect.Class where

import Control.Category (identity)
import Control.Monad (class Monad)
import Effect (Effect)

-- | The `MonadEffect` class captures those monads which support native effects.
-- |
-- | Instances are provided for `Effect` itself, and the standard monad
-- | transformers.
-- |
-- | `liftEffect` can be used in any appropriate monad transformer stack to lift an
-- | action of type `Effect a` into the monad.
-- |
class Monad m <= MonadEffect m where
  liftEffect :: forall a. Effect a -> m a

instance monadEffectEffect :: MonadEffect Effect where
  liftEffect = identity
