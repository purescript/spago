-- | This module defines a data type and various functions for creating and
-- | manipulating JSON values. The README contains additional documentation
-- | for this module.
module Data.Argonaut.Core
  ( Json
  , caseJson
  , caseJsonNull
  , caseJsonBoolean
  , caseJsonNumber
  , caseJsonString
  , caseJsonArray
  , caseJsonObject
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
  , jsonNull
  , jsonTrue
  , jsonFalse
  , jsonZero
  , jsonEmptyString
  , jsonEmptyArray
  , jsonSingletonArray
  , jsonEmptyObject
  , jsonSingletonObject
  , stringify
  , stringifyWithIndent
  ) where

import Prelude

import Data.Function.Uncurried (Fn5, runFn5, Fn7, runFn7)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Obj

-- | The type of JSON data. The underlying representation is the same as what
-- | would be returned from JavaScript's `JSON.parse` function; that is,
-- | ordinary JavaScript booleans, strings, arrays, objects, etc.
foreign import data Json :: Type

instance eqJson :: Eq Json where
  eq j1 j2 = compare j1 j2 == EQ

instance ordJson :: Ord Json where
  compare a b = runFn5 _compare EQ GT LT a b

-- | The type of null values inside JSON data. There is exactly one value of
-- | this type: in JavaScript, it is written `null`. This module exports this
-- | value as `jsonNull`.
foreign import data JNull :: Type

instance eqJNull :: Eq JNull where
  eq _ _ = true

instance ordJNull :: Ord JNull where
  compare _ _ = EQ

-- | Case analysis for `Json` values. See the README for more information.
caseJson
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (Array Json -> a)
  -> (Object Json -> a)
  -> Json
  -> a
caseJson a b c d e f json = runFn7 _caseJson a b c d e f json

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was null, and a default value for all other cases.
caseJsonNull :: forall a. a -> (Unit -> a) -> Json -> a
caseJsonNull d f j = runFn7 _caseJson f (const d) (const d) (const d) (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Boolean`, and a default value for all other cases.
caseJsonBoolean :: forall a. a -> (Boolean -> a) -> Json -> a
caseJsonBoolean d f j = runFn7 _caseJson (const d) f (const d) (const d) (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Number`, and a default value for all other cases.
caseJsonNumber :: forall a. a -> (Number -> a) -> Json -> a
caseJsonNumber d f j = runFn7 _caseJson (const d) (const d) f (const d) (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `String`, and a default value for all other cases.
caseJsonString :: forall a. a -> (String -> a) -> Json -> a
caseJsonString d f j = runFn7 _caseJson (const d) (const d) (const d) f (const d) (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Array Json`, and a default value for all other cases.
caseJsonArray :: forall a. a -> (Array Json -> a) -> Json -> a
caseJsonArray d f j = runFn7 _caseJson (const d) (const d) (const d) (const d) f (const d) j

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was an `Object`, and a default value for all other cases.
caseJsonObject :: forall a. a -> (Object Json -> a) -> Json -> a
caseJsonObject d f j = runFn7 _caseJson (const d) (const d) (const d) (const d) (const d) f j

verbJsonType :: forall a b. b -> (a -> b) -> (b -> (a -> b) -> Json -> b) -> Json -> b
verbJsonType def f g = g def f

-- Tests

isJsonType :: forall a. (Boolean -> (a -> Boolean) -> Json -> Boolean) -> Json -> Boolean
isJsonType = verbJsonType false (const true)

-- | Check if the provided `Json` is the `null` value
isNull :: Json -> Boolean
isNull = isJsonType caseJsonNull

-- | Check if the provided `Json` is a `Boolean`
isBoolean :: Json -> Boolean
isBoolean = isJsonType caseJsonBoolean

-- | Check if the provided `Json` is a `Number`
isNumber :: Json -> Boolean
isNumber = isJsonType caseJsonNumber

-- | Check if the provided `Json` is a `String`
isString :: Json -> Boolean
isString = isJsonType caseJsonString

-- | Check if the provided `Json` is an `Array`
isArray :: Json -> Boolean
isArray = isJsonType caseJsonArray

-- | Check if the provided `Json` is an `Object`
isObject :: Json -> Boolean
isObject = isJsonType caseJsonObject

-- Decoding

toJsonType
  :: forall a
   . (Maybe a -> (a -> Maybe a) -> Json -> Maybe a)
  -> Json
  -> Maybe a
toJsonType = verbJsonType Nothing Just

-- | Convert `Json` to the `Unit` value if the `Json` is the null value
toNull :: Json -> Maybe Unit
toNull = toJsonType caseJsonNull

-- | Convert `Json` to a `Boolean` value, if the `Json` is a boolean.
toBoolean :: Json -> Maybe Boolean
toBoolean = toJsonType caseJsonBoolean

-- | Convert `Json` to a `Number` value, if the `Json` is a number.
toNumber :: Json -> Maybe Number
toNumber = toJsonType caseJsonNumber

-- | Convert `Json` to a `String` value, if the `Json` is a string. To write a
-- | `Json` value to a JSON string, see `stringify`.
toString :: Json -> Maybe String
toString = toJsonType caseJsonString

-- | Convert `Json` to an `Array` of `Json` values, if the `Json` is an array.
toArray :: Json -> Maybe (Array Json)
toArray = toJsonType caseJsonArray

-- | Convert `Json` to an `Object` of `Json` values, if the `Json` is an object.
toObject :: Json -> Maybe (Object Json)
toObject = toJsonType caseJsonObject

-- Encoding

-- | Construct `Json` from a `Boolean` value
foreign import fromBoolean :: Boolean -> Json

-- | Construct `Json` from a `Number` value
foreign import fromNumber :: Number -> Json

-- | Construct the `Json` representation of a `String` value.
-- | Note that this function only produces `Json` containing a single piece of `String`
-- | data (similar to `fromBoolean`, `fromNumber`, etc.).
-- | This function does NOT convert the `String` encoding of a JSON value to `Json` - For that
-- | purpose, you'll need to use `jsonParser`.
foreign import fromString :: String -> Json

-- | Construct `Json` from an array of `Json` values
foreign import fromArray :: Array Json -> Json

-- | Construct `Json` from an object with `Json` values
foreign import fromObject :: Object Json -> Json

-- Defaults

-- | The JSON null value represented as `Json`
foreign import jsonNull :: Json

-- | The true boolean value represented as `Json`
jsonTrue :: Json
jsonTrue = fromBoolean true

-- | The false boolean value represented as `Json`
jsonFalse :: Json
jsonFalse = fromBoolean false

-- | The number zero represented as `Json`
jsonZero :: Json
jsonZero = fromNumber 0.0

-- | An empty string represented as `Json`
jsonEmptyString :: Json
jsonEmptyString = fromString ""

-- | An empty array represented as `Json`
jsonEmptyArray :: Json
jsonEmptyArray = fromArray []

-- | An empty object represented as `Json`
jsonEmptyObject :: Json
jsonEmptyObject = fromObject Obj.empty

-- | Constructs a `Json` array value containing only the provided value
jsonSingletonArray :: Json -> Json
jsonSingletonArray j = fromArray [ j ]

-- | Constructs a `Json` object value containing only the provided key and value
jsonSingletonObject :: String -> Json -> Json
jsonSingletonObject key val = fromObject (Obj.singleton key val)

-- | Converts a `Json` value to a JSON string. To retrieve a string from a `Json`
-- | string value, see `fromString`.
foreign import stringify :: Json -> String

-- | Converts a `Json` value to a JSON string.
-- | The first `Int` argument specifies the amount of white space characters to use as indentation.
-- | This number is capped at 10 (if it is greater, the value is just 10). Values less than 1 indicate that no space should be used.
foreign import stringifyWithIndent :: Int -> Json -> String

foreign import _caseJson
  :: forall z
   . Fn7
       (Unit -> z)
       (Boolean -> z)
       (Number -> z)
       (String -> z)
       (Array Json -> z)
       (Object Json -> z)
       Json
       z

foreign import _compare :: Fn5 Ordering Ordering Ordering Json Json Ordering
