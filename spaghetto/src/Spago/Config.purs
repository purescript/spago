module Spago.Config where

import Spago.Prelude

import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Either as Either
import Data.Map as Map
import Foreign.Object (Object)
import Foreign.Object as Object
import Yaml as Yaml

-- TODO: this module needs stronger types
type Config =
  { name :: String
  , dependencies :: Dependencies
  , packages_db :: PackagesDb
  }

newtype Dependencies = Dependencies (Map String (Maybe String))

instance Yaml.ToYaml Dependencies where
  encode (Dependencies deps) = Core.fromArray
    $ Array.fromFoldable
    $ Map.mapMaybeWithKey
        ( \name maybeRange -> Just $ case maybeRange of
            Nothing -> Core.fromString name
            Just range -> Core.jsonSingletonObject name (Core.fromString range)
        )
        deps
  decode =
    let
      b :: Object Core.Json -> Either String (Tuple String (Maybe String))
      b obj = do
        o <- traverse Yaml.decode obj
        Either.note "Expected Object here" $ Object.toAscUnfoldable o

      c :: String -> Either String (Tuple String (Maybe String))
      c str = Right (Tuple str Nothing)
    in
      Core.caseJsonArray (Left "Expected Array of Dependencies")
        ( \arrayOfJson -> map
            (Dependencies <<< Map.fromFoldable)
            (for arrayOfJson \el -> (Core.caseJsonString (Left "Expected String Dependency") c el) <|> (Core.caseJsonObject (Left "Expected Object Dependency") b el))
        )

type PackagesDb =
  { set :: String
  , extra_packages :: Map String ExtraPackage
  }

-- TODO: local packages too
type ExtraPackage =
  { git :: String
  , ref :: String
  }

-- TODO alternateBackend
-- TODO publish config
