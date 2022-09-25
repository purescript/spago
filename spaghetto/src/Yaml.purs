module Yaml
  ( printYaml
  , stringifyYaml
  , parseYaml
  , writeYamlFile
  , writeYamlDocFile
  , readYamlFile
  , readYamlDocFile
  , encodeObject
  , insert
  , (:=)
  , get
  , (.:)
  , getOptional
  , (.:?)
  , roundtrip
  , class StringEncodable
  , toEncodableString
  , fromEncodableString
  , class ToYaml
  , encode
  , decode
  -- Required for record instances, but not intended for use in user code
  , class EncodeRecord
  , encodeRecord
  , class DecodeRecord
  , decodeRecord
  , class EncodeRecordField
  , encodeRecordField
  , class DecodeRecordField
  , decodeRecordField
  , YamlDoc
  ) where

import Prelude

import Control.Monad.State (State, runState)
import Control.Monad.State as State
import Data.Argonaut.Core as Core
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(..), note)
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Symbol (class IsSymbol)
import Data.Symbol as Symbol
import Data.Traversable (traverse)
import Data.Tuple (Tuple, snd)
import Effect.Aff (Aff, try)
import Effect.Aff as Aff
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.SPDX (License)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Parsing as Parsing
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Registry.Hash (Sha256)
import Registry.Json as Registry.Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Type.Proxy (Proxy(..))

foreign import yamlParserImpl :: forall a. Fn3 (String -> a) (Core.Json -> a) String a

foreign import yamlDocParserImpl :: forall a b. Fn3 (String -> a) (YamlDoc b -> a) String a

foreign import toJsonImpl :: forall a. Fn1 (YamlDoc a) Core.Json

foreign import toStringImpl :: forall a. Fn1 (YamlDoc a) String

foreign import data YamlDoc :: Type -> Type

instance Show (YamlDoc a) where
  show = toStringImpl

-- | Parse a JSON string, constructing the `Toml` value described by the string.
-- | To convert a string into a `Toml` string, see `fromString`.
yamlParser :: forall a. String -> Either String (YamlDoc a)
yamlParser j = runFn3 yamlDocParserImpl Left Right j

-- | Converts a `Toml` value to a JSON string. To retrieve a string from a `Toml`
-- | string value, see `fromString`.
foreign import stringify :: Core.Json -> String

-- | Converts a `Json` value to a JSON string.
-- | The first `Int` argument specifies the amount of white space characters to use as indentation.
-- | This number is capped at 10 (if it is greater, the value is just 10). Values less than 1 indicate that no space should be used.
foreign import stringifyWithIndent :: Int -> Core.Json -> String

-- | Print a type as a formatted YAML string
printYaml :: forall a. ToYaml a => a -> String
printYaml = stringifyWithIndent 2 <<< encode

-- | Print a type as a YAML string without formatting
stringifyYaml :: forall a. ToYaml a => a -> String
stringifyYaml = stringify <<< encode

-- | Parse a type from a string of YAML data.
parseYamlDoc :: forall a. ToYaml a => String -> Either String { doc :: YamlDoc a, yaml :: a }
parseYamlDoc yamlStr = do
  doc <- yamlParser yamlStr
  yaml <- decode (runFn1 toJsonImpl doc)
  pure { doc, yaml }

parseYaml :: forall a. ToYaml a => String -> Either String a
parseYaml = parseYamlDoc >>> map _.yaml

-- | Encode data as formatted YAML and write it to the provided filepath
writeYamlFile :: forall a. ToYaml a => FilePath -> a -> Aff Unit
writeYamlFile path = FS.writeTextFile UTF8 path <<< (_ <> "\n") <<< printYaml

-- | Decode data from a YAML file at the provided filepath
readYamlFile :: forall a. ToYaml a => FilePath -> Aff (Either String a)
readYamlFile path = do
  result <- try $ FS.readTextFile UTF8 path
  pure (lmap Aff.message result >>= parseYaml)

writeYamlDocFile :: forall a. ToYaml a => FilePath -> YamlDoc a -> Aff Unit
writeYamlDocFile path = FS.writeTextFile UTF8 path <<< (_ <> "\n") <<< runFn1 toStringImpl

readYamlDocFile :: forall a. ToYaml a => FilePath -> Aff (Either String { doc :: YamlDoc a, yaml :: a })
readYamlDocFile path = do
  result <- try $ FS.readTextFile UTF8 path
  pure (lmap Aff.message result >>= parseYamlDoc)

-- | Encode a YAML object by running a series of calls to `putField`
encodeObject :: State (Object Core.Json) Unit -> Core.Json
encodeObject encoder = Core.fromObject $ snd $ runState encoder Object.empty

-- | Encode and insert a value into a YAML object at the specified key. If the
-- | encoded value is `null` then it will be omitted.
insert :: forall a. ToYaml a => String -> a -> State (Object Core.Json) Unit
insert key value = do
  let encoded = encode value
  unless (Core.isNull encoded) do
    State.modify_ (Object.insert key encoded)

infix 7 insert as :=

-- | Look up and decode a field in an object, failing if it is not there.
get :: forall a. ToYaml a => Object Core.Json -> String -> Either String a
get object key = maybe (Left $ "Expected value at key: '" <> key <> "'") decode (Object.lookup key object)

infix 7 get as .:

-- | Look up and decode a field in an object, returning `Maybe` if it is not there.
getOptional :: forall a. ToYaml a => Object Core.Json -> String -> Either String (Maybe a)
getOptional object key = maybe (pure Nothing) decode' (Object.lookup key object)
  where
  decode' yaml = if Core.isNull yaml then pure Nothing else map Just (decode yaml)

infix 7 getOptional as .:?

roundtrip :: forall a. ToYaml a => a -> Either String a
roundtrip = encode >>> decode

-- | A class for values that can be encoded as YAML strings. This class is used
-- | for values that may be used as map keys, which are encoded as objects.
class StringEncodable a where
  toEncodableString :: a -> String
  fromEncodableString :: String -> Either String a

instance StringEncodable String where
  toEncodableString = identity
  fromEncodableString = Right

instance StringEncodable PackageName where
  toEncodableString = PackageName.print
  fromEncodableString = lmap Parsing.parseErrorMessage <<< PackageName.parse

-- | A class for encoding and decoding YAML
class ToYaml a where
  encode :: a -> Core.Json
  decode :: Core.Json -> Either String a

instance ToYaml Core.Json where
  encode = identity
  decode = Right

instance ToYaml Boolean where
  encode = Core.fromBoolean
  decode = Core.caseJsonBoolean (Left "Expected Boolean") Right

instance ToYaml String where
  encode = Core.fromString
  decode = Core.caseJsonString (Left "Expected String") Right

instance ToYaml PackageName where
  encode = Registry.Json.encode
  decode = Registry.Json.decode

instance ToYaml Version where
  encode = Registry.Json.encode
  decode = Registry.Json.decode

instance ToYaml License where
  encode = Registry.Json.encode
  decode = Registry.Json.decode

instance ToYaml Sha256 where
  encode = Registry.Json.encode
  decode = Registry.Json.decode

instance ToYaml Number where
  encode = Core.fromNumber
  decode = Core.caseJsonNumber (Left "Expected Number") Right

instance ToYaml Int where
  encode = Core.fromNumber <<< Int.toNumber
  decode = note "Expected Int" <<< Int.fromNumber <=< decode

instance ToYaml a => ToYaml (Array a) where
  encode = Core.fromArray <<< map encode
  decode = Core.caseJsonArray (Left "Expected Array") (traverse decode)

instance ToYaml a => ToYaml (Object a) where
  -- We intentionally do not sort objects here so as to preserve insertion order
  encode = Core.fromObject <<< map encode
  decode = Core.caseJsonObject (Left "Expected Object") (traverse decode)

instance ToYaml a => ToYaml (Maybe a) where
  encode = case _ of
    Nothing -> Core.jsonNull
    Just value -> encode value
  decode yaml
    | Core.isNull yaml = Right Nothing
    | otherwise = map Just $ decode yaml

instance (ToYaml e, ToYaml a) => ToYaml (Either e a) where
  encode = encode <<< case _ of
    Left e -> { tag: "Left", value: encode e }
    Right v -> { tag: "Right", value: encode v }
  decode yaml = do
    obj <- decode yaml
    tag <- obj .: "tag"
    value <- obj .: "value"
    case tag of
      Just "Right" -> Right <$> decode value
      Just "Left" -> Left <$> decode value
      _ -> Left $ "Expected { tag: <Right|Left>, value: <value> }, got: " <> show { tag, value: Core.stringify value }

instance ToYaml NonEmptyString where
  encode = encode <<< NES.toString
  decode = decode >=> NES.fromString >>> note "Expected NonEmptyString"

instance ToYaml a => ToYaml (NonEmptyArray a) where
  encode = encode <<< NEA.toArray
  decode = decode >=> NEA.fromArray >>> note "Expected NonEmptyArray"

instance (Ord k, StringEncodable k, ToYaml v) => ToYaml (Map k v) where
  encode = encode <<< Object.fromFoldable <<< toTupleArray
    where
    toTupleArray :: Map k v -> Array (Tuple String v)
    toTupleArray = map (lmap toEncodableString) <<< Map.toUnfoldable

  decode = toMap <=< decode
    where
    toMap :: Object v -> Either String (Map k v)
    toMap = map (Map.fromFoldable :: Array _ -> _) <<< traverse (ltraverse fromEncodableString) <<< Object.toAscUnfoldable

instance (EncodeRecord row list, DecodeRecord row list, RL.RowToList row list) => ToYaml (Record row) where
  encode record = encode $ Object.fromFoldable $ sortObject $ encodeRecord record (Proxy :: Proxy list)
    where
    sortObject :: Object Core.Json -> Array (Tuple String Core.Json)
    sortObject = Object.toAscUnfoldable

  decode yaml = case Core.toObject yaml of
    Nothing -> Left "Expected Object"
    Just object -> decodeRecord object (Proxy :: Proxy list)

---------

class EncodeRecord (row :: Row Type) (list :: RL.RowList Type) where
  encodeRecord :: Record row -> Proxy list -> Object Core.Json

instance EncodeRecord row RL.Nil where
  encodeRecord _ _ = Object.empty

instance (EncodeRecordField value, ToYaml value, EncodeRecord row tail, IsSymbol field, Row.Cons field value tail' row) => EncodeRecord row (RL.Cons field value tail) where
  encodeRecord row _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = Symbol.reflectSymbol _field
      fieldValue = Record.get _field row
      object = encodeRecord row (Proxy :: Proxy tail)

    encodeRecordField fieldName fieldValue object

class DecodeRecord (row :: Row Type) (list :: RL.RowList Type) | list -> row where
  decodeRecord :: Object Core.Json -> Proxy list -> Either String (Record row)

instance DecodeRecord () RL.Nil where
  decodeRecord _ _ = Right {}

instance (DecodeRecordField value, DecodeRecord rowTail tail, IsSymbol field, Row.Cons field value rowTail row, Row.Lacks field rowTail) => DecodeRecord row (RL.Cons field value tail) where
  decodeRecord object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = Symbol.reflectSymbol _field

    case decodeRecordField (Object.lookup fieldName object) of
      Nothing -> Left $ "Expected field: '" <> fieldName <> "'"
      Just fieldValue -> do
        val <- fieldValue
        rest <- decodeRecord object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

-- This class ensures that `Nothing` fields are not included when encoding records
class EncodeRecordField a where
  encodeRecordField :: String -> a -> Object Core.Json -> Object Core.Json

instance ToYaml a => EncodeRecordField (Maybe a) where
  encodeRecordField key = case _ of
    Nothing -> identity
    Just value -> Object.insert key (encode value)

else instance ToYaml a => EncodeRecordField a where
  encodeRecordField key value = Object.insert key (encode value)

-- This class ensures that missing and null values are decoded via `Maybe`, and
-- otherwise defers to calls to `decode`.
class DecodeRecordField a where
  decodeRecordField :: Maybe Core.Json -> Maybe (Either String a)

instance ToYaml a => DecodeRecordField (Maybe a) where
  decodeRecordField = Just <<< case _ of
    Nothing -> Right Nothing
    Just yaml | Core.isNull yaml -> Right Nothing
    Just yaml -> decode yaml

else instance ToYaml a => DecodeRecordField a where
  decodeRecordField = map decode
