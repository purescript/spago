module Control.Monad.ST.Internal
  ( Region
  , ST
  , run
  , while
  , for
  , foreach
  , STRef
  , new
  , read
  , modify'
  , modify
  , write
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Partial.Unsafe (unsafePartial)

-- | `ST` is concerned with _restricted_ mutation. Mutation is restricted to a
-- | _region_ of mutable references. This kind is inhabited by phantom types
-- | which represent regions in the type system.
foreign import data Region :: Type

-- | The `ST` type constructor allows _local mutation_, i.e. mutation which
-- | does not "escape" into the surrounding computation.
-- |
-- | An `ST` computation is parameterized by a phantom type which is used to
-- | restrict the set of reference cells it is allowed to access.
-- |
-- | The `run` function can be used to run a computation in the `ST` monad.
foreign import data ST :: Region -> Type -> Type

type role ST nominal representational

foreign import map_ :: forall r a b. (a -> b) -> ST r a -> ST r b

foreign import pure_ :: forall r a. a -> ST r a

foreign import bind_ :: forall r a b. ST r a -> (a -> ST r b) -> ST r b

instance functorST :: Functor (ST r) where
  map = map_

instance applyST :: Apply (ST r) where
  apply = ap

instance applicativeST :: Applicative (ST r) where
  pure = pure_

instance bindST :: Bind (ST r) where
  bind = bind_

instance monadST :: Monad (ST r)

instance monadRecST :: MonadRec (ST r) where
  tailRecM f a = do
    r <- new =<< f a
    while (isLooping <$> read r) do
      read r >>= case _ of
        Loop a' -> do
          e <- f a'
          void (write e r)
        Done _ -> pure unit
    fromDone <$> read r
    where
      fromDone :: forall a b. Step a b -> b
      fromDone = unsafePartial \(Done b) -> b

      isLooping = case _ of
        Loop _ -> true
        _ -> false

instance semigroupST :: Semigroup a => Semigroup (ST r a) where
  append = lift2 append

instance monoidST :: Monoid a => Monoid (ST r a) where
  mempty = pure mempty

-- | Run an `ST` computation.
-- |
-- | Note: the type of `run` uses a rank-2 type to constrain the phantom
-- | type `r`, such that the computation must not leak any mutable references
-- | to the surrounding computation. It may cause problems to apply this
-- | function using the `$` operator. The recommended approach is to use
-- | parentheses instead.
foreign import run :: forall a. (forall r. ST r a) -> a

-- | Loop while a condition is `true`.
-- |
-- | `while b m` is ST computation which runs the ST computation `b`. If its
-- | result is `true`, it runs the ST computation `m` and loops. If not, the
-- | computation ends.
foreign import while :: forall r a. ST r Boolean -> ST r a -> ST r Unit

-- | Loop over a consecutive collection of numbers
-- |
-- | `ST.for lo hi f` runs the computation returned by the function `f` for each
-- | of the inputs between `lo` (inclusive) and `hi` (exclusive).
foreign import for :: forall r a. Int -> Int -> (Int -> ST r a) -> ST r Unit

-- | Loop over an array of values.
-- |
-- | `ST.foreach xs f` runs the computation returned by the function `f` for each
-- | of the inputs `xs`.
foreign import foreach :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit

-- | The type `STRef r a` represents a mutable reference holding a value of
-- | type `a`, which can be used with the `ST r` effect.
foreign import data STRef :: Region -> Type -> Type

type role STRef nominal representational

-- | Create a new mutable reference.
foreign import new :: forall a r. a -> ST r (STRef r a)

-- | Read the current value of a mutable reference.
foreign import read :: forall a r. STRef r a -> ST r a

-- | Update the value of a mutable reference by applying a function
-- | to the current value, computing a new state value for the reference and
-- | a return value.
modify' :: forall r a b. (a -> { state :: a, value :: b }) -> STRef r a -> ST r b
modify' = modifyImpl

foreign import modifyImpl :: forall r a b. (a -> { state :: a, value :: b }) -> STRef r a -> ST r b

-- | Modify the value of a mutable reference by applying a function to the
-- | current value. The modified value is returned.
modify :: forall r a. (a -> a) -> STRef r a -> ST r a
modify f = modify' \s -> let s' = f s in { state: s', value: s' }

-- | Set the value of a mutable reference.
foreign import write :: forall a r. a -> STRef r a -> ST r a
