module Data.Exists where

import Unsafe.Coerce (unsafeCoerce)

-- | This type constructor can be used to existentially quantify over a type.
-- |
-- | Specifically, the type `Exists f` is isomorphic to the existential type `exists a. f a`.
-- |
-- | Existential types can be encoded using universal types (`forall`) for endofunctors in more general
-- | categories. The benefit of this library is that, by using the FFI, we can create an efficient
-- | representation of the existential by simply hiding type information.
-- |
-- | For example, consider the type `exists s. Tuple s (s -> Tuple s a)` which represents infinite streams
-- | of elements of type `a`.
-- |
-- | This type can be constructed by creating a type constructor `StreamF` as follows:
-- |
-- | ```purescript
-- | data StreamF a s = StreamF s (s -> Tuple s a)
-- | ```
-- |
-- | We can then define the type of streams using `Exists`:
-- |
-- | ```purescript
-- | type Stream a = Exists (StreamF a)
-- | ```
foreign import data Exists :: forall k. (k -> Type) -> Type

type role Exists representational

-- | The `mkExists` function is used to introduce a value of type `Exists f`, by providing a value of
-- | type `f a`, for some type `a` which will be hidden in the existentially-quantified type.
-- |
-- | For example, to create a value of type `Stream Number`, we might use `mkExists` as follows:
-- |
-- | ```purescript
-- | nats :: Stream Number
-- | nats = mkExists $ StreamF 0 (\n -> Tuple (n + 1) n)
-- | ```
mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

-- | The `runExists` function is used to eliminate a value of type `Exists f`. The rank 2 type ensures
-- | that the existentially-quantified type does not escape its scope. Since the function is required
-- | to work for _any_ type `a`, it will work for the existentially-quantified type.
-- |
-- | For example, we can write a function to obtain the head of a stream by using `runExists` as follows:
-- |
-- | ```purescript
-- | head :: forall a. Stream a -> a
-- | head = runExists head'
-- |   where
-- |   head' :: forall s. StreamF a s -> a
-- |   head' (StreamF s f) = snd (f s)
-- | ```
runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce
