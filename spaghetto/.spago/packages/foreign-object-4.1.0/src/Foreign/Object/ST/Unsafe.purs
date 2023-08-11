module Foreign.Object.ST.Unsafe where

import Control.Monad.ST (ST)
import Foreign.Object (Object)
import Foreign.Object.ST (STObject)

-- | Unsafely get the object out of ST without copying it
-- |
-- | If you later change the ST version of the map the pure value will also change.
foreign import unsafeFreeze :: forall a r. STObject r a -> ST r (Object a)
