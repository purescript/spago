module Record.Unsafe.Union where

import Data.Function.Uncurried (Fn2, runFn2)

foreign import unsafeUnionFn :: forall r1 r2 r3. Fn2 (Record r1) (Record r2) (Record r3)

unsafeUnion :: forall r1 r2 r3. Record r1 -> Record r2 -> Record r3
unsafeUnion = runFn2 unsafeUnionFn
