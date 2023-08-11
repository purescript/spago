module Control.Extend
  ( class Extend, extend, (<<=), extendFlipped, (=>>)
  , composeCoKleisli, (=>=)
  , composeCoKleisliFlipped, (=<=)
  , duplicate
  , module Data.Functor
  ) where

import Control.Category (identity)

import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Semigroup (class Semigroup, (<>))

-- | The `Extend` class defines the extension operator `(<<=)`
-- | which extends a local context-dependent computation to
-- | a global computation.
-- |
-- | `Extend` is the dual of `Bind`, and `(<<=)` is the dual of
-- | `(>>=)`.
-- |
-- | Laws:
-- |
-- | - Associativity: `extend f <<< extend g = extend (f <<< extend g)`
class Functor w <= Extend w where
  extend :: forall b a. (w a -> b) -> w a -> w b

instance extendFn :: Semigroup w => Extend ((->) w) where
  extend f g w = f \w' -> g (w <> w')

foreign import arrayExtend :: forall a b. (Array a -> b) -> Array a -> Array b

instance extendArray :: Extend Array where
  extend = arrayExtend

infixr 1 extend as <<=

-- | A version of `extend` with its arguments flipped.
extendFlipped :: forall b a w. Extend w => w a -> (w a -> b) -> w b
extendFlipped w f = f <<= w

infixl 1 extendFlipped as =>>

-- | Forwards co-Kleisli composition.
composeCoKleisli :: forall b a w c. Extend w => (w a -> b) -> (w b -> c) -> w a -> c
composeCoKleisli f g w = g (f <<= w)

infixr 1 composeCoKleisli as =>=

-- | Backwards co-Kleisli composition.
composeCoKleisliFlipped :: forall b a w c. Extend w => (w b -> c) -> (w a -> b) -> w a -> c
composeCoKleisliFlipped f g w = f (g <<= w)

infixr 1 composeCoKleisliFlipped as =<=

-- | Duplicate a comonadic context.
-- |
-- | `duplicate` is dual to `Control.Bind.join`.
duplicate :: forall a w. Extend w => w a -> w (w a)
duplicate = extend identity
