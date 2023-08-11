module Foreign.Object.Unsafe
  ( unsafeIndex
  ) where

import Foreign.Object (Object)

-- | Unsafely get the value for a key in a object.
-- |
-- | This function does not check whether the key exists in the object.
foreign import unsafeIndex :: forall a. Object a -> String -> a
