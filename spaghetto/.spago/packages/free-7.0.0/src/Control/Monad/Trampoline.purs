-- | A _trampoline_ monad, which can be used at the bottom of
-- | a monad transformer stack to avoid stack overflows in large
-- | monadic computations.

module Control.Monad.Trampoline
  ( Trampoline
  , done
  , delay
  , runTrampoline
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFree)


-- | The `Trampoline` monad
-- |
-- | A computation of type `Trampoline a` consists of zero or more lazy
-- | suspensions before a value is returned.
type Trampoline = Free ((->) Unit)

-- | Return a value immediately
done :: forall a. a -> Trampoline a
done = pure

-- | Use the `Trampoline` monad to represent the delayed evaluation of a value.
delay :: forall a. (Unit -> a) -> Trampoline a
delay = liftF

-- | Run a computation in the `Trampoline` monad.
runTrampoline :: forall a. Trampoline a -> a
runTrampoline = runFree (_ $ unit)
