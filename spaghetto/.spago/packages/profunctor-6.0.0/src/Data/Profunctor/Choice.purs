module Data.Profunctor.Choice where

import Prelude

import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor, dimap)

-- | The `Choice` class extends `Profunctor` with combinators for working with
-- | sum types.
-- |
-- | `left` and `right` lift values in a `Profunctor` to act on the `Left` and
-- | `Right` components of a sum, respectively.
-- |
-- | Looking at `Choice` through the intuition of inputs and outputs
-- | yields the following type signature:
-- | ```
-- | left ::  forall input output a. p input output -> p (Either input a) (Either output a)
-- | right :: forall input output a. p input output -> p (Either a input) (Either a output)
-- | ```
-- | If we specialize the profunctor `p` to the `function` arrow, we get the following type
-- | signatures:
-- | ```
-- | left ::  forall input output a. (input -> output) -> (Either input a) -> (Either output a)
-- | right :: forall input output a. (input -> output) -> (Either a input) -> (Either a output)
-- | ```
-- | When the `profunctor` is `Function` application, `left` allows you to map a function over the
-- | left side of an `Either`, and `right` maps it over the right side (same as `map` would do).
class Profunctor p <= Choice p where
  left :: forall a b c. p a b -> p (Either a c) (Either b c)
  right :: forall a b c. p b c -> p (Either a b) (Either a c)

instance choiceFn :: Choice (->) where
  left a2b (Left a)  = Left $ a2b a
  left _   (Right c) = Right c
  right = (<$>)

-- | Compose a value acting on a sum from two values, each acting on one of
-- | the components of the sum.
-- |
-- | Specializing `(+++)` to function application would look like this:
-- | ```
-- | (+++) :: forall a b c d. (a -> b) -> (c -> d) -> (Either a c) -> (Either b d)
-- | ```
-- | We take two functions, `f` and `g`, and we transform them into a single function which
-- | takes an `Either`and maps `f` over the left side and `g` over the right side.  Just like
-- | `bi-map` would do for the `bi-functor` instance of `Either`.
splitChoice
  :: forall p a b c d
   . Category p
  => Choice p
  => p a b
  -> p c d
  -> p (Either a c) (Either b d)
splitChoice l r = left l >>> right r

infixr 2 splitChoice as +++

-- | Compose a value which eliminates a sum from two values, each eliminating
-- | one side of the sum.
-- |
-- | This combinator is useful when assembling values from smaller components,
-- | because it provides a way to support two different types of input.
-- |
-- | Specializing `(|||)` to function application would look like this:
-- | ```
-- | (|||) :: forall a b c d. (a -> c) -> (b -> c) -> Either a b -> c
-- | ```
-- | We take two functions, `f` and `g`, which both return the same type `c` and we transform them into a
-- | single function which takes an `Either` value with the parameter type of `f` on the left side and
-- | the parameter type of `g` on the right side. The function then runs either `f` or `g`, depending on
-- | whether the `Either` value is a `Left` or a `Right`.
-- | This allows us to bundle two different computations which both have the same result type into one
-- | function which will run the approriate computation based on the parameter supplied in the `Either` value.
fanin
  :: forall p a b c
   . Category p
  => Choice p
  => p a c
  -> p b c
  -> p (Either a b) c
fanin l r = (l +++ r) >>> join
  where
  join :: p (Either c c) c
  join = dimap (either identity identity) identity identity

infixr 2 fanin as |||
