-- | Utilities for working with partial functions.
-- | See the README for more documentation.
module Partial.Unsafe
  ( unsafePartial
  , unsafeCrashWith
  ) where

import Partial (crashWith)

-- Note: this function's type signature is more like
-- `(Unit -> a) -> a`. However, we would need to use
-- `unsafeCoerce` to make this compile, incurring
-- either a dependency or reimplementing it here.
-- Rather than doing that, we'll use a type signature
-- of `a -> b` instead.
foreign import _unsafePartial :: forall a b. a -> b

-- | Discharge a partiality constraint, unsafely.
unsafePartial :: forall a. (Partial => a) -> a
unsafePartial = _unsafePartial

-- | A function which crashes with the specified error message.
unsafeCrashWith :: forall a. String -> a
unsafeCrashWith msg = unsafePartial (crashWith msg)
