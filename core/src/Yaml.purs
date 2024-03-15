module Spago.Yaml
  ( YamlDoc
  , parseYaml
  , parseYamlDoc
  , printYaml
  , stringifyYaml
  , toString
  , toJson
  , parser
  ) where

import Prelude

import Data.Argonaut.Core as Core
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)

foreign import yamlParserImpl :: forall a. Fn3 (String -> a) (Core.Json -> a) String a

foreign import yamlDocParserImpl :: forall a b. Fn3 (String -> a) (YamlDoc b -> a) String a

foreign import toJsonImpl :: forall a. Fn1 (YamlDoc a) Core.Json

foreign import toStringImpl :: forall a. Fn1 (YamlDoc a) String

foreign import data YamlDoc :: Type -> Type

parser :: forall a. String -> Either String (YamlDoc a)
parser yaml = runFn3 yamlDocParserImpl Left Right yaml

toString :: forall a. YamlDoc a -> String
toString = runFn1 toStringImpl

toJson :: forall a. YamlDoc a -> Core.Json
toJson = runFn1 toJsonImpl

-- | Converts a `Toml` value to a JSON string. To retrieve a string from a `Toml`
-- | string value, see `fromString`.
foreign import stringify :: Core.Json -> String

-- | Converts a `Json` value to a JSON string.
-- | The first `Int` argument specifies the amount of white space characters to use as indentation.
-- | This number is capped at 10 (if it is greater, the value is just 10). Values less than 1 indicate that no space should be used.
foreign import stringifyWithIndent :: Int -> Core.Json -> String

-- | Print a type as a formatted YAML string
printYaml :: forall a. JsonCodec a -> a -> String
printYaml codec = stringifyWithIndent 2 <<< CA.encode codec

-- | Print a type as a YAML string without formatting
stringifyYaml :: forall a. JsonCodec a -> a -> String
stringifyYaml codec = stringify <<< CA.encode codec

-- | Parse a type from a string of JSON data.
parseYaml :: forall a. JsonCodec a -> String -> Either JsonDecodeError a
parseYaml codec = parseYamlDoc codec >>> map _.yaml

-- | Parse a type from a string of YAML data.
parseYamlDoc :: forall a. JsonCodec a -> String -> Either JsonDecodeError { doc :: YamlDoc a, yaml :: a }
parseYamlDoc codec yamlStr = do
  doc <- lmap (\err -> CA.TypeMismatch ("YAML: " <> err)) (parser yamlStr)
  yaml <- CA.decode codec (toJson doc)
  pure { doc, yaml }
