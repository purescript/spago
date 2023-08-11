module Effect.Exception.Unsafe where

import Effect.Exception (Error, error, throwException)
import Effect.Unsafe (unsafePerformEffect)
import Control.Semigroupoid ((<<<))

-- | Throw an exception in pure code. This function should be used very
-- | sparingly, as it can cause unexpected crashes at runtime.
unsafeThrowException :: forall a. Error -> a
unsafeThrowException = unsafePerformEffect <<< throwException

-- | Defined as `unsafeThrowException <<< error`.
unsafeThrow :: forall a. String -> a
unsafeThrow = unsafeThrowException <<< error
