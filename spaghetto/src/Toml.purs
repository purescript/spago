module Toml
  ( module Exports
  , printToml
  , stringifyToml
  , parseToml
  , writeTomlFile
  , readTomlFile
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
  , class ToToml
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
  ) where

import Prelude

import Control.Monad.State (State, runState)
import Control.Monad.State as State
import Toml.Core (Toml, stringify) as Exports
import Toml.Core as Core
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(..), note)
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
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | Print a type as a formatted TOML string
printToml :: forall a. ToToml a => a -> String
printToml = Core.stringify <<< encode

-- | Print a type as a TOML string without formatting
stringifyToml :: forall a. ToToml a => a -> String
stringifyToml = Core.stringify <<< encode

-- | Parse a type from a string of TOML data.
parseToml :: forall a. ToToml a => String -> Either String a
parseToml = decode <=< Core.tomlParser

-- | Encode data as formatted TOML and write it to the provided filepath
writeTomlFile :: forall a. ToToml a => FilePath -> a -> Aff Unit
writeTomlFile path = FS.writeTextFile UTF8 path <<< (_ <> "\n") <<< printToml

-- | Decode data from a TOML file at the provided filepath
readTomlFile :: forall a. ToToml a => FilePath -> Aff (Either String a)
readTomlFile path = do
  result <- try $ FS.readTextFile UTF8 path
  pure (lmap Aff.message result >>= parseToml)

-- | Encode a TOML object by running a series of calls to `putField`
encodeObject :: State (Object Core.Toml) Unit -> Core.Toml
encodeObject encoder = Core.fromObject $ snd $ runState encoder Object.empty

-- | Encode and insert a value into a TOML object at the specified key. If the
-- | encoded value is `null` then it will be omitted.
insert :: forall a. ToToml a => String -> a -> State (Object Core.Toml) Unit
insert key value = do
  let encoded = encode value
  unless (Core.isNull encoded) do
    State.modify_ (Object.insert key encoded)

infix 7 insert as :=

-- | Look up and decode a field in an object, failing if it is not there.
get :: forall a. ToToml a => Object Core.Toml -> String -> Either String a
get object key = maybe (Left $ "Expected value at key: '" <> key <> "'") decode (Object.lookup key object)

infix 7 get as .:

-- | Look up and decode a field in an object, returning `Maybe` if it is not there.
getOptional :: forall a. ToToml a => Object Core.Toml -> String -> Either String (Maybe a)
getOptional object key = maybe (pure Nothing) decode' (Object.lookup key object)
  where
  decode' toml = if Core.isNull toml then pure Nothing else map Just (decode toml)

infix 7 getOptional as .:?

roundtrip :: forall a. ToToml a => a -> Either String a
roundtrip = encode >>> decode

-- | A class for values that can be encoded as TOML strings. This class is used
-- | for values that may be used as map keys, which are encoded as objects.
class StringEncodable a where
  toEncodableString :: a -> String
  fromEncodableString :: String -> Either String a

instance StringEncodable String where
  toEncodableString = identity
  fromEncodableString = Right

-- | A class for encoding and decoding TOML
class ToToml a where
  encode :: a -> Core.Toml
  decode :: Core.Toml -> Either String a

instance ToToml Core.Toml where
  encode = identity
  decode = Right

instance ToToml Boolean where
  encode = Core.fromBoolean
  decode = Core.caseTomlBoolean (Left "Expected Boolean") Right

instance ToToml String where
  encode = Core.fromString
  decode = Core.caseTomlString (Left "Expected String") Right

instance ToToml Number where
  encode = Core.fromNumber
  decode = Core.caseTomlNumber (Left "Expected Number") Right

instance ToToml Int where
  encode = Core.fromNumber <<< Int.toNumber
  decode = note "Expected Int" <<< Int.fromNumber <=< decode

instance ToToml a => ToToml (Array a) where
  encode = Core.fromArray <<< map encode
  decode = Core.caseTomlArray (Left "Expected Array") (traverse decode)

instance ToToml a => ToToml (Object a) where
  -- We intentionally do not sort objects here so as to preserve insertion order
  encode = Core.fromObject <<< map encode
  decode = Core.caseTomlObject (Left "Expected Object") (traverse decode)

instance ToToml a => ToToml (Maybe a) where
  encode = case _ of
    Nothing -> Core.tomlNull
    Just value -> encode value
  decode toml
    | Core.isNull toml = Right Nothing
    | otherwise = map Just $ decode toml

instance (ToToml e, ToToml a) => ToToml (Either e a) where
  encode = encode <<< case _ of
    Left e -> { tag: "Left", value: encode e }
    Right v -> { tag: "Right", value: encode v }
  decode toml = do
    obj <- decode toml
    tag <- obj .: "tag"
    value <- obj .: "value"
    case tag of
      Just "Right" -> Right <$> decode value
      Just "Left" -> Left <$> decode value
      _ -> Left $ "Expected { tag: <Right|Left>, value: <value> }, got: " <> show { tag, value: Core.stringify value }

instance ToToml NonEmptyString where
  encode = encode <<< NES.toString
  decode = decode >=> NES.fromString >>> note "Expected NonEmptyString"

instance ToToml a => ToToml (NonEmptyArray a) where
  encode = encode <<< NEA.toArray
  decode = decode >=> NEA.fromArray >>> note "Expected NonEmptyArray"

instance (Ord k, StringEncodable k, ToToml v) => ToToml (Map k v) where
  encode = encode <<< Object.fromFoldable <<< toTupleArray
    where
    toTupleArray :: Map k v -> Array (Tuple String v)
    toTupleArray = map (lmap toEncodableString) <<< Map.toUnfoldable

  decode = toMap <=< decode
    where
    toMap :: Object v -> Either String (Map k v)
    toMap = map (Map.fromFoldable :: Array _ -> _) <<< traverse (ltraverse fromEncodableString) <<< Object.toAscUnfoldable

instance (EncodeRecord row list, DecodeRecord row list, RL.RowToList row list) => ToToml (Record row) where
  encode record = encode $ Object.fromFoldable $ sortObject $ encodeRecord record (Proxy :: Proxy list)
    where
    sortObject :: Object Core.Toml -> Array (Tuple String Core.Toml)
    sortObject = Object.toAscUnfoldable

  decode toml = case Core.toObject toml of
    Nothing -> Left "Expected Object"
    Just object -> decodeRecord object (Proxy :: Proxy list)

---------

class EncodeRecord (row :: Row Type) (list :: RL.RowList Type) where
  encodeRecord :: Record row -> Proxy list -> Object Core.Toml

instance EncodeRecord row RL.Nil where
  encodeRecord _ _ = Object.empty

instance (EncodeRecordField value, ToToml value, EncodeRecord row tail, IsSymbol field, Row.Cons field value tail' row) => EncodeRecord row (RL.Cons field value tail) where
  encodeRecord row _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = Symbol.reflectSymbol _field
      fieldValue = Record.get _field row
      object = encodeRecord row (Proxy :: Proxy tail)

    encodeRecordField fieldName fieldValue object

class DecodeRecord (row :: Row Type) (list :: RL.RowList Type) | list -> row where
  decodeRecord :: Object Core.Toml -> Proxy list -> Either String (Record row)

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
  encodeRecordField :: String -> a -> Object Core.Toml -> Object Core.Toml

instance ToToml a => EncodeRecordField (Maybe a) where
  encodeRecordField key = case _ of
    Nothing -> identity
    Just value -> Object.insert key (encode value)

else instance ToToml a => EncodeRecordField a where
  encodeRecordField key value = Object.insert key (encode value)

-- This class ensures that missing and null values are decoded via `Maybe`, and
-- otherwise defers to calls to `decode`.
class DecodeRecordField a where
  decodeRecordField :: Maybe Core.Toml -> Maybe (Either String a)

instance ToToml a => DecodeRecordField (Maybe a) where
  decodeRecordField = Just <<< case _ of
    Nothing -> Right Nothing
    Just toml | Core.isNull toml -> Right Nothing
    Just toml -> decode toml

else instance ToToml a => DecodeRecordField a where
  decodeRecordField = map decode
