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

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)
import JSON (JSON)

foreign import yamlParserImpl :: forall a. Fn3 (String -> a) (JSON -> a) String a

foreign import yamlDocParserImpl :: forall a b. Fn3 (String -> a) (YamlDoc b -> a) String a

foreign import toJsonImpl :: forall a. Fn1 (YamlDoc a) JSON

foreign import toStringImpl :: forall a. Fn1 (YamlDoc a) String

foreign import data YamlDoc :: Type -> Type

parser :: forall a. String -> Either String (YamlDoc a)
parser yaml = runFn3 yamlDocParserImpl Left Right yaml

toString :: forall a. YamlDoc a -> String
toString = runFn1 toStringImpl

toJson :: forall a. YamlDoc a -> JSON
toJson = runFn1 toJsonImpl

-- | Converts a `Toml` value to a JSON string. To retrieve a string from a `Toml`
-- | string value, see `fromString`.
foreign import stringify :: JSON -> String

-- | Converts a `Json` value to a JSON string.
-- | The first `Int` argument specifies the amount of white space characters to use as indentation.
-- | This number is capped at 10 (if it is greater, the value is just 10). Values less than 1 indicate that no space should be used.
foreign import stringifyWithIndent :: Int -> JSON -> String

-- | Print a type as a formatted YAML string
printYaml :: forall a. CJ.Codec a -> a -> String
printYaml codec = stringifyWithIndent 2 <<< CJ.encode codec

-- | Print a type as a YAML string without formatting
stringifyYaml :: forall a. CJ.Codec a -> a -> String
stringifyYaml codec = stringify <<< CJ.encode codec

-- | Parse a type from a string of JSON data.
parseYaml :: forall a. CJ.Codec a -> String -> Either CJ.DecodeError a
parseYaml codec = parseYamlDoc codec >>> map _.yaml

-- | Parse a type from a string of YAML data.
parseYamlDoc :: forall a. CJ.Codec a -> String -> Either CJ.DecodeError { doc :: YamlDoc a, yaml :: a }
parseYamlDoc codec yamlStr = do
  doc <- lmap (\err -> CJ.DecodeError.basic ("YAML: " <> err)) (parser yamlStr)
  yaml <- CJ.decode codec (toJson doc)
  pure { doc, yaml }
