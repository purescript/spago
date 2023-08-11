-- | This module defines a type class for types which act like
-- | _property indices_.

module Foreign.Index
  ( class Index
  , class Indexable
  , readProp
  , readIndex
  , ix, (!)
  , index
  , hasProperty
  , hasOwnProperty
  , errorAt
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)

import Foreign (Foreign, ForeignError(..), typeOf, isUndefined, isNull, fail)
import Data.Function.Uncurried (Fn2, runFn2, Fn4, runFn4)
import Data.List.NonEmpty (NonEmptyList)

-- | This type class identifies types that act like _property indices_.
-- |
-- | The canonical instances are for `String`s and `Int`s.
class Index i m | i -> m where
  index :: Foreign -> i -> ExceptT (NonEmptyList ForeignError) m Foreign
  hasProperty :: i -> Foreign -> Boolean
  hasOwnProperty :: i -> Foreign -> Boolean
  errorAt :: i -> ForeignError -> ForeignError

class Indexable a m | a -> m where
  ix :: forall i. Index i m => a -> i -> ExceptT (NonEmptyList ForeignError) m Foreign

infixl 9 ix as !

foreign import unsafeReadPropImpl :: forall r k. Fn4 r (Foreign -> r) k Foreign r

unsafeReadProp :: forall k m. Monad m => k -> Foreign -> ExceptT (NonEmptyList ForeignError) m Foreign
unsafeReadProp k value =
  runFn4 unsafeReadPropImpl (fail (TypeMismatch "object" (typeOf value))) pure k value

-- | Attempt to read a value from a foreign value property
readProp :: forall m. Monad m => String -> Foreign -> ExceptT (NonEmptyList ForeignError) m Foreign
readProp = unsafeReadProp

-- | Attempt to read a value from a foreign value at the specified numeric index
readIndex :: forall m. Monad m => Int -> Foreign -> ExceptT (NonEmptyList ForeignError) m Foreign
readIndex = unsafeReadProp

foreign import unsafeHasOwnProperty :: forall k. Fn2 k Foreign Boolean

hasOwnPropertyImpl :: forall k. k -> Foreign -> Boolean
hasOwnPropertyImpl _ value | isNull value = false
hasOwnPropertyImpl _ value | isUndefined value = false
hasOwnPropertyImpl p value | typeOf value == "object" || typeOf value == "function" = runFn2 unsafeHasOwnProperty p value
hasOwnPropertyImpl _ _ = false

foreign import unsafeHasProperty :: forall k. Fn2 k Foreign Boolean

hasPropertyImpl :: forall k. k -> Foreign -> Boolean
hasPropertyImpl _ value | isNull value = false
hasPropertyImpl _ value | isUndefined value = false
hasPropertyImpl p value | typeOf value == "object" || typeOf value == "function" = runFn2 unsafeHasProperty p value
hasPropertyImpl _ _ = false

instance indexString :: Monad m => Index String m where
  index = flip readProp
  hasProperty = hasPropertyImpl
  hasOwnProperty = hasOwnPropertyImpl
  errorAt = ErrorAtProperty

instance indexInt :: Monad m => Index Int m where
  index = flip readIndex
  hasProperty = hasPropertyImpl
  hasOwnProperty = hasOwnPropertyImpl
  errorAt = ErrorAtIndex

instance indexableForeign :: Monad m => Indexable Foreign m where
  ix = index

instance indexableExceptT :: Monad m => Indexable (ExceptT (NonEmptyList ForeignError) m Foreign) m where
  ix f i = flip index i =<< f
