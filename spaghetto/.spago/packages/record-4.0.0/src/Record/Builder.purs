module Record.Builder
  ( Builder
  , build
  , buildFromScratch
  , flip
  , insert
  , modify
  , delete
  , rename
  , merge
  , union
  , disjointUnion
  , nub
  ) where

import Prelude hiding (flip)

import Data.Function (flip) as Function
import Data.Function.Uncurried (runFn2)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Record.Unsafe.Union (unsafeUnionFn)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

foreign import copyRecord :: forall r1. Record r1 -> Record r1
foreign import unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2
foreign import unsafeModify :: forall a b r1 r2. String -> (a -> b) -> Record r1 -> Record r2
foreign import unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
foreign import unsafeRename :: forall r1 r2. String -> String -> Record r1 -> Record r2

-- | A `Builder` can be used to `build` a record by incrementally adding
-- | fields in-place, instead of using `insert` and repeatedly generating new
-- | immutable records which need to be garbage collected.
-- |
-- | The mutations accumulated in a `Builder` are safe because intermediate states can't be
-- | observed. These mutations, then, are performed all-at-once in the `build` function.
-- |
-- | The `Category` instance for `Builder` can be used to compose builders.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build (insert x 42 >>> insert y "testing") {} :: { x :: Int, y :: String }
-- | ```
newtype Builder a b = Builder (a -> b)

-- | Build a record, starting from some other record.
build :: forall r1 r2. Builder (Record r1) (Record r2) -> Record r1 -> Record r2
build (Builder b) r1 = b (copyRecord r1)

-- | Build a record from scratch.
buildFromScratch :: forall r. Builder (Record ()) (Record r) -> Record r
buildFromScratch = Function.flip build {}

-- | Flip a function of one argument returning a builder.
flip :: forall r1 r2 r3. (Record r1 -> Builder (Record r2) (Record r3)) -> Record r2 -> Builder (Record r1) (Record r3)
flip f b = Builder \a -> build (f a) b

derive newtype instance semigroupoidBuilder :: Semigroupoid Builder
derive newtype instance categoryBuilder :: Category Builder

-- | Build by inserting a new field.
insert
  :: forall l a r1 r2
   . Row.Cons l a r1 r2
  => Row.Lacks l r1
  => IsSymbol l
  => Proxy l
  -> a
  -> Builder (Record r1) (Record r2)
insert l a = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1

-- | Build by modifying an existing field.
modify
  :: forall l a b r r1 r2
   . Row.Cons l a r r1
  => Row.Cons l b r r2
  => IsSymbol l
  => Proxy l
  -> (a -> b)
  -> Builder (Record r1) (Record r2)
modify l f = Builder \r1 -> unsafeModify (reflectSymbol l) f r1

-- | Build by deleting an existing field.
delete
  :: forall l a r1 r2
   . IsSymbol l
   => Row.Lacks l r1
   => Row.Cons l a r1 r2
   => Proxy l
   -> Builder (Record r2) (Record r1)
delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2

-- | Build by renaming an existing field.
rename :: forall l1 l2 a r1 r2 r3
   . IsSymbol l1
  => IsSymbol l2
  => Row.Cons l1 a r2 r1
  => Row.Lacks l1 r2
  => Row.Cons l2 a r2 r3
  => Row.Lacks l2 r2
  => Proxy l1
  -> Proxy l2
  -> Builder (Record r1) (Record r3)
rename l1 l2 = Builder \r1 -> unsafeRename (reflectSymbol l1) (reflectSymbol l2) r1

-- | Build by merging existing fields from another record, taking precedence
-- | in the case of overlaps.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build (merge { x: 1, y: "y" }) { y: 2, z: true }
-- |  :: { x :: Int, y :: String, z :: Boolean }
-- | ```
merge
  :: forall r1 r2 r3 r4
   . Row.Union r1 r2 r3
  => Row.Nub r3 r4
  => Record r1
  -> Builder (Record r2) (Record r4)
merge r1 = Builder \r2 -> runFn2 unsafeUnionFn r1 r2

-- | Build by merging existing fields from another record, taking precedence
-- | in the case of overlaps. Unlike `merge`, this does not remove duplicate
-- | labels from the resulting record type. This can result in better inference
-- | for some pipelines, deferring the need for a `Nub` constraint.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build (union { x: 1, y: "y" }) { y: 2, z: true }
-- |  :: { x :: Int, y :: String, y :: Int, z :: Boolean }
-- | ```
union
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => Record r1
  -> Builder (Record r2) (Record r3)
union r1 = Builder \r2 -> runFn2 unsafeUnionFn r1 r2

-- | Build by merging some disjoint set of fields from another record.
disjointUnion
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => Row.Nub r3 r3
  => Record r1
  -> Builder (Record r2) (Record r3)
disjointUnion r1 = Builder \r2 -> runFn2 unsafeUnionFn r1 r2

-- | A coercion which removes duplicate labels from a record's type.
nub
  :: forall r1 r2
   . Row.Nub r1 r2
  => Builder (Record r1) (Record r2)
nub = Builder unsafeCoerce
