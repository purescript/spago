module TODO where

import Partial.Unsafe

todo :: forall a. a
todo = unsafeCrashWith "todo"
