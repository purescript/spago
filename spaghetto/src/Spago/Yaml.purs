module Spago.Yaml
  ( yamlParser
  , toString
  , stringifyWithIndent
  , stringify
  , toJson
  , YamlDoc
  ) where

import Data.Argonaut.Core as Core
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)

foreign import yamlParserImpl :: forall a. Fn3 (String -> a) (Core.Json -> a) String a

foreign import yamlDocParserImpl :: forall a b. Fn3 (String -> a) (YamlDoc b -> a) String a

foreign import toJsonImpl :: forall a. Fn1 (YamlDoc a) Core.Json

foreign import toStringImpl :: forall a. Fn1 (YamlDoc a) String

foreign import data YamlDoc :: Type -> Type

-- | Parse a JSON string, constructing the `Toml` value described by the string.
-- | To convert a string into a `Toml` string, see `fromString`.
yamlParser :: forall a. String -> Either String (YamlDoc a)
yamlParser j = runFn3 yamlDocParserImpl Left Right j

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

