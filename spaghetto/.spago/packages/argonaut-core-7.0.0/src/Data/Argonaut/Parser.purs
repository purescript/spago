module Data.Argonaut.Parser (jsonParser) where

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

foreign import _jsonParser :: forall a. Fn3 (String -> a) (Json -> a) String a

-- | Parse a JSON string, constructing the `Json` value described by the string.
-- | To convert a string into a `Json` string, see `fromString`.
jsonParser :: String -> Either String Json
jsonParser j = runFn3 _jsonParser Left Right j
