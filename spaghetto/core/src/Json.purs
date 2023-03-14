module Spago.Json where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either)

-- | Print a type as a formatted JSON string
printJson :: forall a. JsonCodec a -> a -> String
printJson codec = Argonaut.stringifyWithIndent 2 <<< CA.encode codec

-- | Print a type as a JSON string without formatting
stringifyJson :: forall a. JsonCodec a -> a -> String
stringifyJson codec = Argonaut.stringify <<< CA.encode codec

-- | Parse a type from a string of JSON data.
parseJson :: forall a. JsonCodec a -> String -> Either JsonDecodeError a
parseJson codec = CA.decode codec <=< lmap (\err -> CA.TypeMismatch ("JSON: " <> err)) <<< Argonaut.Parser.jsonParser
