-- | This module provides the `Effect` type, which is used to represent
-- | _native_ effects. The `Effect` type provides a typed API for effectful
-- | computations, while at the same time generating efficient JavaScript.
module Effect
  ( Effect
  , untilE, whileE, forE, foreachE
  ) where

import Prelude

import Control.Apply (lift2)

-- | A native effect. The type parameter denotes the return type of running the
-- | effect, that is, an `Effect Int` is a possibly-effectful computation which
-- | eventually produces a value of the type `Int` when it finishes.
foreign import data Effect :: Type -> Type

type role Effect representational

instance functorEffect :: Functor Effect where
  map = liftA1

instance applyEffect :: Apply Effect where
  apply = ap

instance applicativeEffect :: Applicative Effect where
  pure = pureE

foreign import pureE :: forall a. a -> Effect a

instance bindEffect :: Bind Effect where
  bind = bindE

foreign import bindE :: forall a b. Effect a -> (a -> Effect b) -> Effect b

instance monadEffect :: Monad Effect

-- | The `Semigroup` instance for effects allows you to run two effects, one
-- | after the other, and then combine their results using the result type's
-- | `Semigroup` instance.
instance semigroupEffect :: Semigroup a => Semigroup (Effect a) where
  append = lift2 append

-- | If you have a `Monoid a` instance, then `mempty :: Effect a` is defined as
-- | `pure mempty`.
instance monoidEffect :: Monoid a => Monoid (Effect a) where
  mempty = pureE mempty

-- | Loop until a condition becomes `true`.
-- |
-- | `untilE b` is an effectful computation which repeatedly runs the effectful
-- | computation `b`, until its return value is `true`.
foreign import untilE :: Effect Boolean -> Effect Unit

-- | Loop while a condition is `true`.
-- |
-- | `whileE b m` is effectful computation which runs the effectful computation
-- | `b`. If its result is `true`, it runs the effectful computation `m` and
-- | loops. If not, the computation ends.
foreign import whileE :: forall a. Effect Boolean -> Effect a -> Effect Unit

-- | Loop over a consecutive collection of numbers.
-- |
-- | `forE lo hi f` runs the computation returned by the function `f` for each
-- | of the inputs between `lo` (inclusive) and `hi` (exclusive).
foreign import forE :: Int -> Int -> (Int -> Effect Unit) -> Effect Unit

-- | Loop over an array of values.
-- |
-- | `foreachE xs f` runs the computation returned by the function `f` for each
-- | of the inputs `xs`.
foreign import foreachE :: forall a. Array a -> (a -> Effect Unit) -> Effect Unit
