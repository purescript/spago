module Data.Profunctor.Strong where

import Prelude

import Data.Profunctor (class Profunctor, dimap)
import Data.Tuple (Tuple(..))

-- | The `Strong` class extends `Profunctor` with combinators for working with
-- | product types.
-- |
-- | `first` and `second` lift values in a `Profunctor` to act on the first and
-- | second components of a `Tuple`, respectively.
-- |
-- | Another way to think about Strong is to piggyback on the intuition of
-- | inputs and outputs.  Rewriting the type signature in this light then yields:
-- | ```
-- | first ::  forall input output a. p input output -> p (Tuple input a) (Tuple output a)
-- | second :: forall input output a. p input output -> p (Tuple a input) (Tuple a output)
-- | ```
-- | If we specialize the profunctor p to the function arrow, we get the following type
-- | signatures, which may look a bit more familiar:
-- | ```
-- | first ::  forall input output a. (input -> output) -> (Tuple input a) -> (Tuple output a)
-- | second :: forall input output a. (input -> output) -> (Tuple a input) -> (Tuple a output)
-- | ```
-- | So, when the `profunctor` is `Function` application, `first` essentially applies your function
-- | to the first element of a `Tuple`, and `second` applies it to the second element (same as `map` would do).
class Profunctor p <= Strong p where
  first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c)
  second :: forall a b c. p b c -> p (Tuple a b) (Tuple a c)

instance strongFn :: Strong (->) where
  first a2b (Tuple a c) = Tuple (a2b a) c
  second = (<$>)

-- | Compose a value acting on a `Tuple` from two values, each acting on one of
-- | the components of the `Tuple`.
-- |
-- | Specializing `(***)` to function application would look like this:
-- | ```
-- | (***) :: forall a b c d. (a -> b) -> (c -> d) -> (Tuple a c) -> (Tuple b d)
-- | ```
-- | We take two functions, `f` and `g`, and we transform them into a single function which
-- | takes a `Tuple` and maps `f` over the first element and `g` over the second.  Just like `bi-map`
-- | would do for the `bi-functor` instance of `Tuple`.
splitStrong
  :: forall p a b c d
   . Category p
  => Strong p
  => p a b
  -> p c d
  -> p (Tuple a c) (Tuple b d)
splitStrong l r = first l >>> second r

infixr 3 splitStrong as ***

-- | Compose a value which introduces a `Tuple` from two values, each introducing
-- | one side of the `Tuple`.
-- |
-- | This combinator is useful when assembling values from smaller components,
-- | because it provides a way to support two different types of output.
-- |
-- | Specializing `(&&&)` to function application would look like this:
-- | ```
-- | (&&&) :: forall a b c. (a -> b) -> (a -> c) -> (a -> (Tuple b c))
-- | ```
-- | We take two functions, `f` and `g`, with the same parameter type and we transform them into a
-- | single function which takes one parameter and returns a `Tuple` of the results of running
-- | `f` and `g` on the parameter, respectively.  This allows us to run two parallel computations
-- | on the same input and return both results in a `Tuple`.
fanout
  :: forall p a b c
   . Category p
  => Strong p
  => p a b
  -> p a c
  -> p a (Tuple b c)
fanout l r = split >>> (l *** r)
  where
  split :: p a (Tuple a a)
  split = dimap identity (\a -> Tuple a a) identity

infixr 3 fanout as &&&
