-- | This module defines types and functions for working with nullable types
-- | using the FFI.

module Data.Nullable
  ( Nullable
  , null
  , notNull
  , toMaybe
  , toNullable
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.Function (on)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)

-- | A nullable type. This type constructor is intended to be used for
-- | interoperating with JavaScript functions which accept or return null
-- | values.
-- |
-- | The runtime representation of `Nullable T` is the same as that of `T`,
-- | except that it may also be `null`. For example, the JavaScript values
-- | `null`, `[]`, and `[1,2,3]` may all be given the type
-- | `Nullable (Array Int)`. Similarly, the JavaScript values `[]`, `[null]`,
-- | and `[1,2,null,3]` may all be given the type `Array (Nullable Int)`.
-- |
-- | There is one pitfall with `Nullable`, which is that values of the type
-- | `Nullable T` will not function as you might expect if the type `T` happens
-- | to itself permit `null` as a valid runtime representation.
-- |
-- | In particular, values of the type `Nullable (Nullable T)` will ‘collapse’,
-- | in the sense that the PureScript expressions `notNull null` and `null`
-- | will both leave you with a value whose runtime representation is just
-- | `null`. Therefore it is important to avoid using `Nullable T` in
-- | situations where `T` itself can take `null` as a runtime representation.
-- | If in doubt, use `Maybe` instead.
-- |
-- | `Nullable` does not permit lawful `Functor`, `Applicative`, or `Monad`
-- | instances as a result of this pitfall, which is why these instances are
-- | not provided.
foreign import data Nullable :: Type -> Type

type role Nullable representational

-- | The null value.
foreign import null :: forall a. Nullable a

foreign import nullable :: forall a r. Fn3 (Nullable a) r (a -> r) r

-- | Wrap a non-null value.
foreign import notNull :: forall a. a -> Nullable a

-- | Takes `Nothing` to `null`, and `Just a` to `a`.
toNullable :: forall a. Maybe a -> Nullable a
toNullable = maybe null notNull

-- | Represent `null` using `Maybe a` as `Nothing`. Note that this function
-- | can violate parametricity, as it inspects the runtime representation of
-- | its argument (see the warning about the pitfall of `Nullable` above).
toMaybe :: forall a. Nullable a -> Maybe a
toMaybe n = runFn3 nullable n Nothing Just

instance showNullable :: Show a => Show (Nullable a) where
  show = maybe "null" show <<< toMaybe

instance eqNullable :: Eq a => Eq (Nullable a) where
  eq = eq `on` toMaybe

instance eq1Nullable :: Eq1 Nullable where
  eq1 = eq

instance ordNullable :: Ord a => Ord (Nullable a) where
  compare = compare `on` toMaybe

instance ord1Nullable :: Ord1 Nullable where
  compare1 = compare
