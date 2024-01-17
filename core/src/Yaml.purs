module Spago.Yaml
  ( YamlDoc
  , parseYaml
  , parseYamlDoc
  , printYaml
  , stringifyYaml
  , toString
  , YamlMigrationStep(..)
  ) where

import Prelude hiding (add)

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Uncurried (STFn2, STFn3, runSTFn2, runSTFn3)
import Data.Argonaut.Core as Core
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.List (List)
import Data.Maybe (Maybe, maybe)

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

-- | Print a type as a formatted YAML string
printYaml :: forall a. JsonCodec a -> a -> String
printYaml codec = stringifyWithIndent 2 <<< CA.encode codec

-- | Print a type as a YAML string without formatting
stringifyYaml :: forall a. JsonCodec a -> a -> String
stringifyYaml codec = stringify <<< CA.encode codec

-- | Parse a type from a string of JSON data.
parseYaml :: forall a. JsonCodec a -> Array (List YamlMigrationStep) -> String -> Either JsonDecodeError a
parseYaml codec migrations = parseYamlDoc codec migrations >>> map _.yaml

-- | Parse a type from a string of YAML data.
parseYamlDoc :: forall a. JsonCodec a -> Array (List YamlMigrationStep) -> String -> Either JsonDecodeError { doc :: YamlDoc a, yaml :: a }
parseYamlDoc codec migrations yamlStr = do
  doc <- lmap (\err -> CA.TypeMismatch ("YAML: " <> err)) (yamlParser yamlStr)
  yaml <- CA.decode codec $ toJson $ runYamlMigrations doc migrations
  pure { doc, yaml }

foreign import getImpl :: forall h a. STFn3 (YamlDoc a) String Boolean h (YamlDoc a)

get :: forall h a. YamlDoc a -> String -> Boolean -> ST h (YamlDoc a)
get doc path keep = runSTFn3 getImpl doc path keep

foreign import addImpl :: forall h a. STFn3 (YamlDoc a) String (YamlDoc a) h Unit

add :: forall h a. YamlDoc a -> String -> YamlDoc a -> ST h Unit
add doc path value = runSTFn3 addImpl doc path value

foreign import deleteImpl :: forall h a. STFn2 (YamlDoc a) String h Unit

delete :: forall h a. YamlDoc a -> String -> ST h Unit
delete doc path = runSTFn2 deleteImpl doc path

foreign import hasImpl :: forall a. Fn2 (YamlDoc a) String Boolean

has :: forall a. YamlDoc a -> String -> Boolean
has doc path = runFn2 hasImpl doc path

foreign import toSeqItems :: forall a. YamlDoc a -> Array (YamlDoc a)

foreign import isMap :: forall a. YamlDoc a -> Boolean

newtype YamlMigrationStep = YamlMigrationStep
  { key :: String
  , toPrevKey :: Maybe (String -> String)
  , updateValue :: List (YamlMigrationStep)
  }

runYamlMigrations :: forall a. YamlDoc a -> Array (List YamlMigrationStep) -> YamlDoc a
runYamlMigrations doc migrations = ST.run (foldl migrationSteps (pure doc) migrations)

migrationSteps :: forall h a. ST h (YamlDoc a) -> List YamlMigrationStep -> ST h (YamlDoc a)
migrationSteps doc updates = foldl migrationStep doc updates

migrationStep :: forall h a. ST h (YamlDoc a) -> YamlMigrationStep -> ST h (YamlDoc a)
migrationStep stDoc (YamlMigrationStep { key, toPrevKey, updateValue}) = do
  doc <- stDoc
  let oldKey = maybe key (\f -> f key) toPrevKey
  when (has doc oldKey) do
    value <- get doc oldKey true
    for_ (toSeqItems value) \item ->
      void $ migrationSteps (pure item) updateValue
    when (isMap value) do
      void $ migrationSteps (pure value) updateValue             
    for_ toPrevKey \_ -> do
      delete doc oldKey
      add doc key value
  pure doc