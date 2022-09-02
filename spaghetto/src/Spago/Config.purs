module Spago.Config where

import Spago.Prelude

import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Map as Map
import Foreign.Object (Object)
import Foreign.Object as Object
import Parsing as Parsing
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Spago.PackageSet (GitPackage)
import Yaml as Yaml

-- TODO: this module needs stronger types?
type Config =
  { name :: PackageName
  , dependencies :: Dependencies
  , packages_db :: PackagesDb
  }

newtype Dependencies = Dependencies (Map PackageName (Maybe String))

instance Yaml.ToYaml Dependencies where
  encode (Dependencies deps) = Core.fromArray
    $ Array.fromFoldable
    $ Map.mapMaybeWithKey
        ( \name maybeRange -> Just $ case maybeRange of
            Nothing -> Core.fromString (PackageName.print name)
            Just range -> Core.jsonSingletonObject (PackageName.print name) (Core.fromString range)
        )
        deps
  decode =
    let
      decodePkgWithRange :: Object Core.Json -> Either String (Tuple PackageName (Maybe String))
      decodePkgWithRange obj = do
        o <- traverse Yaml.decode obj
        let maybeTuple = Object.toAscUnfoldable o
        case maybeTuple of
          Nothing -> Left "Expected Object here"
          Just (Tuple rawPkg range) -> do
            pkgName <- lmap Parsing.parseErrorMessage $ PackageName.parse rawPkg
            Right (Tuple pkgName range)

      decodePkg :: String -> Either String (Tuple PackageName (Maybe String))
      decodePkg str = case PackageName.parse str of
        Left e -> Left (Parsing.parseErrorMessage e)
        Right p -> Right (Tuple p Nothing)
    in
      Core.caseJsonArray (Left "Expected Array of Dependencies")
        ( \arrayOfJson -> map
            (Dependencies <<< Map.fromFoldable)
            ( for arrayOfJson \el ->
                (Core.caseJsonString (Left "Expected String Dependency") decodePkg el)
                  <|> (Core.caseJsonObject (Left "Expected Object Dependency") decodePkgWithRange el)
            )
        )

type PackagesDb =
  { set :: String
  , extra_packages :: Map PackageName GitPackage
  }

-- TODO alternateBackend
-- TODO publish config
