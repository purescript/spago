module Spago.Json where

import Prelude

import Codec.JSON.DecodeError (DecodeError(..))
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.Either (Either)
import JSON as JSON
import JSON.Path as JSON.Path

-- | Print a type as a formatted JSON string
printJson :: forall a. CJ.Codec a -> a -> String
printJson codec = JSON.printIndented <<< CJ.encode codec

-- | Print a type as a JSON string without formatting
stringifyJson :: forall a. CJ.Codec a -> a -> String
stringifyJson codec = JSON.print <<< CJ.encode codec

-- | Parse a type from a string of JSON data.
parseJson :: forall a. CJ.Codec a -> String -> Either CJ.DecodeError a
parseJson codec = CJ.decode codec <=< lmap (\err -> CJ.DecodeError.basic ("JSON: " <> err)) <<< JSON.parse

-- | Pretty print a DecodeError that happened while parsing a spago.yaml file
printConfigError :: CJ.DecodeError -> Array String
printConfigError (DecodeError { path, message, causes }) =
  case Array.null causes of
    -- If there are causes then we can just throw away the current message and recurse in
    false -> Array.foldMap printConfigError causes
    -- If there are none, then we have reached a leaf, and can print the actual error
    true -> [ JSON.Path.print path <> ": " <> message ]
