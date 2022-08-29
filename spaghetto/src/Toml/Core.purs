-- | This module defines a data type and various functions for creating and
-- | manipulating JSON values. The README contains additional documentation
-- | for this module.
module Toml.Core
  ( Toml
  , caseToml
  , caseTomlNull
  , caseTomlBoolean
  , caseTomlNumber
  , caseTomlString
  , caseTomlArray
  , caseTomlObject
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isArray
  , isObject
  , fromBoolean
  , fromNumber
  , fromString
  , fromArray
  , fromObject
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  , tomlParser
  , tomlNull
  , tomlTrue
  , tomlFalse
  , tomlZero
  , tomlEmptyString
  , tomlEmptyArray
  , tomlSingletonArray
  , tomlEmptyObject
  , tomlSingletonObject
  , stringify
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3, Fn5, runFn5, Fn7, runFn7)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Obj

-- | The type of JSON data. The underlying representation is the same as what
-- | would be returned from JavaScript's `JSON.parse` function; that is,
-- | ordinary JavaScript booleans, strings, arrays, objects, etc.
foreign import data Toml :: Type

instance eqToml :: Eq Toml where
  eq j1 j2 = compare j1 j2 == EQ

instance ordToml :: Ord Toml where
  compare a b = runFn5 _compare EQ GT LT a b

-- | The type of null values inside JSON data. There is exactly one value of
-- | this type: in JavaScript, it is written `null`. This module exports this
-- | value as `tomlNull`.
foreign import data JNull :: Type

instance eqJNull :: Eq JNull where
  eq _ _ = true

instance ordJNull :: Ord JNull where
  compare _ _ = EQ

foreign import _tomlParser :: forall a. Fn3 (String -> a) (Toml -> a) String a

-- | Parse a JSON string, constructing the `Toml` value described by the string.
-- | To convert a string into a `Toml` string, see `fromString`.
tomlParser :: String -> Either String Toml
tomlParser j = runFn3 _tomlParser Left Right j

-- | Case analysis for `Toml` values. See the README for more information.
caseToml
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (Array Toml -> a)
  -> (Object Toml -> a)
  -> Toml
  -> a
caseToml a b c d e f toml = runFn7 _caseToml a b c d e f toml

-- | A simpler version of `caseToml` which accepts a callback for when the
-- | `Toml` argument was null, and a default value for all other cases.
caseTomlNull :: forall a. a -> (Unit -> a) -> Toml -> a
caseTomlNull d f j = runFn7 _caseToml f (const d) (const d) (const d) (const d) (const d) j

-- | A simpler version of `caseToml` which accepts a callback for when the
-- | `Toml` argument was a `Boolean`, and a default value for all other cases.
caseTomlBoolean :: forall a. a -> (Boolean -> a) -> Toml -> a
caseTomlBoolean d f j = runFn7 _caseToml (const d) f (const d) (const d) (const d) (const d) j

-- | A simpler version of `caseToml` which accepts a callback for when the
-- | `Toml` argument was a `Number`, and a default value for all other cases.
caseTomlNumber :: forall a. a -> (Number -> a) -> Toml -> a
caseTomlNumber d f j = runFn7 _caseToml (const d) (const d) f (const d) (const d) (const d) j

-- | A simpler version of `caseToml` which accepts a callback for when the
-- | `Toml` argument was a `String`, and a default value for all other cases.
caseTomlString :: forall a. a -> (String -> a) -> Toml -> a
caseTomlString d f j = runFn7 _caseToml (const d) (const d) (const d) f (const d) (const d) j

-- | A simpler version of `caseToml` which accepts a callback for when the
-- | `Toml` argument was a `Array Toml`, and a default value for all other cases.
caseTomlArray :: forall a. a -> (Array Toml -> a) -> Toml -> a
caseTomlArray d f j = runFn7 _caseToml (const d) (const d) (const d) (const d) f (const d) j

-- | A simpler version of `caseToml` which accepts a callback for when the
-- | `Toml` argument was an `Object`, and a default value for all other cases.
caseTomlObject :: forall a. a -> (Object Toml -> a) -> Toml -> a
caseTomlObject d f j = runFn7 _caseToml (const d) (const d) (const d) (const d) (const d) f j

verbTomlType :: forall a b. b -> (a -> b) -> (b -> (a -> b) -> Toml -> b) -> Toml -> b
verbTomlType def f g = g def f

-- Tests

isTomlType :: forall a. (Boolean -> (a -> Boolean) -> Toml -> Boolean) -> Toml -> Boolean
isTomlType = verbTomlType false (const true)

-- | Check if the provided `Toml` is the `null` value
isNull :: Toml -> Boolean
isNull = isTomlType caseTomlNull

-- | Check if the provided `Toml` is a `Boolean`
isBoolean :: Toml -> Boolean
isBoolean = isTomlType caseTomlBoolean

-- | Check if the provided `Toml` is a `Number`
isNumber :: Toml -> Boolean
isNumber = isTomlType caseTomlNumber

-- | Check if the provided `Toml` is a `String`
isString :: Toml -> Boolean
isString = isTomlType caseTomlString

-- | Check if the provided `Toml` is an `Array`
isArray :: Toml -> Boolean
isArray = isTomlType caseTomlArray

-- | Check if the provided `Toml` is an `Object`
isObject :: Toml -> Boolean
isObject = isTomlType caseTomlObject

-- Decoding

toTomlType
  :: forall a
   . (Maybe a -> (a -> Maybe a) -> Toml -> Maybe a)
  -> Toml
  -> Maybe a
toTomlType = verbTomlType Nothing Just

-- | Convert `Toml` to the `Unit` value if the `Toml` is the null value
toNull :: Toml -> Maybe Unit
toNull = toTomlType caseTomlNull

-- | Convert `Toml` to a `Boolean` value, if the `Toml` is a boolean.
toBoolean :: Toml -> Maybe Boolean
toBoolean = toTomlType caseTomlBoolean

-- | Convert `Toml` to a `Number` value, if the `Toml` is a number.
toNumber :: Toml -> Maybe Number
toNumber = toTomlType caseTomlNumber

-- | Convert `Toml` to a `String` value, if the `Toml` is a string. To write a
-- | `Toml` value to a JSON string, see `stringify`.
toString :: Toml -> Maybe String
toString = toTomlType caseTomlString

-- | Convert `Toml` to an `Array` of `Toml` values, if the `Toml` is an array.
toArray :: Toml -> Maybe (Array Toml)
toArray = toTomlType caseTomlArray

-- | Convert `Toml` to an `Object` of `Toml` values, if the `Toml` is an object.
toObject :: Toml -> Maybe (Object Toml)
toObject = toTomlType caseTomlObject

-- Encoding

-- | Construct `Toml` from a `Boolean` value
foreign import fromBoolean :: Boolean -> Toml

-- | Construct `Toml` from a `Number` value
foreign import fromNumber :: Number -> Toml

-- | Construct the `Toml` representation of a `String` value.
-- | Note that this function only produces `Toml` containing a single piece of `String`
-- | data (similar to `fromBoolean`, `fromNumber`, etc.).
-- | This function does NOT convert the `String` encoding of a JSON value to `Toml` - For that
-- | purpose, you'll need to use `tomlParser`.
foreign import fromString :: String -> Toml

-- | Construct `Toml` from an array of `Toml` values
foreign import fromArray :: Array Toml -> Toml

-- | Construct `Toml` from an object with `Toml` values
foreign import fromObject :: Object Toml -> Toml

-- Defaults

-- | The JSON null value represented as `Toml`
foreign import tomlNull :: Toml

-- | The true boolean value represented as `Toml`
tomlTrue :: Toml
tomlTrue = fromBoolean true

-- | The false boolean value represented as `Toml`
tomlFalse :: Toml
tomlFalse = fromBoolean false

-- | The number zero represented as `Toml`
tomlZero :: Toml
tomlZero = fromNumber 0.0

-- | An empty string represented as `Toml`
tomlEmptyString :: Toml
tomlEmptyString = fromString ""

-- | An empty array represented as `Toml`
tomlEmptyArray :: Toml
tomlEmptyArray = fromArray []

-- | An empty object represented as `Toml`
tomlEmptyObject :: Toml
tomlEmptyObject = fromObject Obj.empty

-- | Constructs a `Toml` array value containing only the provided value
tomlSingletonArray :: Toml -> Toml
tomlSingletonArray j = fromArray [ j ]

-- | Constructs a `Toml` object value containing only the provided key and value
tomlSingletonObject :: String -> Toml -> Toml
tomlSingletonObject key val = fromObject (Obj.singleton key val)

-- | Converts a `Toml` value to a JSON string. To retrieve a string from a `Toml`
-- | string value, see `fromString`.
foreign import stringify :: Toml -> String

foreign import _caseToml
  :: forall z
   . Fn7
       (Unit -> z)
       (Boolean -> z)
       (Number -> z)
       (String -> z)
       (Array Toml -> z)
       (Object Toml -> z)
       Toml
       z

foreign import _compare :: Fn5 Ordering Ordering Ordering Toml Toml Ordering
