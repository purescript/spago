-- | This module defines types and functions for working with _foreign_
-- | data.
-- |
-- | `ExceptT (NonEmptyList ForeignError) m` is used in this library
-- | to encode possible failures when dealing with foreign data.
-- |
-- | The `Alt` instance for `ExceptT` allows us to accumulate errors,
-- | unlike `Either`, which preserves only the last error.
module Foreign
  ( Foreign
  , ForeignError(..)
  , MultipleErrors(..)
  , F
  , FT
  , renderForeignError
  , unsafeToForeign
  , unsafeFromForeign
  , unsafeReadTagged
  , typeOf
  , tagOf
  , isNull
  , isUndefined
  , isArray
  , readString
  , readChar
  , readBoolean
  , readNumber
  , readInt
  , readArray
  , readNull
  , readUndefined
  , readNullOrUndefined
  , fail
  ) where

import Prelude

import Control.Monad.Except (Except, ExceptT, mapExceptT, throwError)
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits (toChar)
import Unsafe.Coerce (unsafeCoerce)

-- | A type for _foreign data_.
-- |
-- | Foreign data is data from any external _unknown_ or _unreliable_
-- | source, for which it cannot be guaranteed that the runtime representation
-- | conforms to that of any particular type.
-- |
-- | Suitable applications of `Foreign` are
-- |
-- | - To represent responses from web services
-- | - To integrate with external JavaScript libraries.
foreign import data Foreign :: Type

-- | A type for foreign type errors
data ForeignError
  = ForeignError String
  | TypeMismatch String String
  | ErrorAtIndex Int ForeignError
  | ErrorAtProperty String ForeignError

derive instance eqForeignError :: Eq ForeignError
derive instance ordForeignError :: Ord ForeignError

instance showForeignError :: Show ForeignError where
  show (ForeignError msg) = "(ForeignError " <> show msg <> ")"
  show (ErrorAtIndex i e) = "(ErrorAtIndex " <> show i <> " " <> show e <> ")"
  show (ErrorAtProperty prop e) = "(ErrorAtProperty " <> show prop <> " " <> show e <> ")"
  show (TypeMismatch exps act) = "(TypeMismatch " <> show exps <> " " <> show act <> ")"

-- | A type for accumulating multiple `ForeignError`s.
type MultipleErrors = NonEmptyList ForeignError

renderForeignError :: ForeignError -> String
renderForeignError (ForeignError msg) = msg
renderForeignError (ErrorAtIndex i e) = "Error at array index " <> show i <> ": " <> renderForeignError e
renderForeignError (ErrorAtProperty prop e) = "Error at property " <> show prop <> ": " <> renderForeignError e
renderForeignError (TypeMismatch exp act) = "Type mismatch: expected " <> exp <> ", found " <> act

-- | While this alias is not deprecated, it is recommended
-- | that one use `Except (NonEmptyList ForeignError)` directly
-- | for all future usages rather than this type alias.
-- |
-- | An error monad, used in this library to encode possible failures when
-- | dealing with foreign data.
-- |
-- | The `Alt` instance for `Except` allows us to accumulate errors,
-- | unlike `Either`, which preserves only the last error.
type F = Except MultipleErrors

-- | While this alias is not deprecated, it is recommended
-- | that one use `ExceptT (NonEmptyList ForeignError)` directly
-- | for all future usages rather than this type alias.
type FT = ExceptT MultipleErrors

-- | Coerce any value to the a `Foreign` value.
-- |
-- | This is considered unsafe as it's only intended to be used on primitive
-- | JavaScript types, rather than PureScript types. Exporting PureScript values
-- | via the FFI can be dangerous as they can be mutated by code outside the
-- | PureScript program, resulting in difficult to diagnose problems elsewhere.
unsafeToForeign :: forall a. a -> Foreign
unsafeToForeign = unsafeCoerce

-- | Unsafely coerce a `Foreign` value.
unsafeFromForeign :: forall a. Foreign -> a
unsafeFromForeign = unsafeCoerce

-- | Read the Javascript _type_ of a value
foreign import typeOf :: Foreign -> String

-- | Read the Javascript _tag_ of a value.
-- |
-- | This function wraps the `Object.toString` method.
foreign import tagOf :: Foreign -> String

-- | Unsafely coerce a `Foreign` value when the value has a particular `tagOf`
-- | value.
unsafeReadTagged :: forall m a. Monad m => String -> Foreign -> ExceptT (NonEmptyList ForeignError) m a
unsafeReadTagged tag value
  | tagOf value == tag = pure (unsafeFromForeign value)
  | otherwise = fail $ TypeMismatch tag (tagOf value)

-- | Test whether a foreign value is null
foreign import isNull :: Foreign -> Boolean

-- | Test whether a foreign value is undefined
foreign import isUndefined :: Foreign -> Boolean

-- | Test whether a foreign value is an array
foreign import isArray :: Foreign -> Boolean

-- | Attempt to coerce a foreign value to a `String`.
readString :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m String
readString = unsafeReadTagged "String"

-- | Attempt to coerce a foreign value to a `Char`.
readChar :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m Char
readChar value = mapExceptT (map $ either (const error) fromString) (readString value)
  where
  fromString = maybe error pure <<< toChar
  error = Left $ NEL.singleton $ TypeMismatch "Char" (tagOf value)

-- | Attempt to coerce a foreign value to a `Boolean`.
readBoolean :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m Boolean
readBoolean = unsafeReadTagged "Boolean"

-- | Attempt to coerce a foreign value to a `Number`.
readNumber :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m Number
readNumber = unsafeReadTagged "Number"

-- | Attempt to coerce a foreign value to an `Int`.
readInt :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m Int
readInt value = mapExceptT (map $ either (const error) fromNumber) (readNumber value)
  where
  fromNumber = maybe error pure <<< Int.fromNumber
  error = Left $ NEL.singleton $ TypeMismatch "Int" (tagOf value)

-- | Attempt to coerce a foreign value to an array.
readArray :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m (Array Foreign)
readArray value
  | isArray value = pure $ unsafeFromForeign value
  | otherwise = fail $ TypeMismatch "array" (tagOf value)

readNull :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m (Maybe Foreign)
readNull value
  | isNull value = pure Nothing
  | otherwise = pure (Just value)

readUndefined :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m (Maybe Foreign)
readUndefined value
  | isUndefined value = pure Nothing
  | otherwise = pure (Just value)

readNullOrUndefined :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m (Maybe Foreign)
readNullOrUndefined value
  | isNull value || isUndefined value = pure Nothing
  | otherwise = pure (Just value)

-- | Throws a failure error in `ExceptT (NonEmptyList ForeignError) m`.
fail :: forall m a. Monad m => ForeignError -> ExceptT (NonEmptyList ForeignError) m a
fail = throwError <<< NEL.singleton
