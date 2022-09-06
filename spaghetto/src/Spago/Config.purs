module Spago.Config where

import Spago.Prelude

import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Map as Map
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.SPDX as Registry
import Parsing as Parsing
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range)
import Registry.Version as Registry.Version
import Spago.PackageSet (GitPackage)
import Yaml as Yaml

type Config =
  { name :: PackageName
  , license :: Registry.License
  , dependencies :: Dependencies
  , packages_db :: PackagesDb
  }

newtype Dependencies = Dependencies (Map PackageName (Maybe Range))

instance Yaml.ToYaml Dependencies where
  encode (Dependencies deps) = Core.fromArray
    $ Array.fromFoldable
    $ Map.mapMaybeWithKey
        ( \name maybeRange -> Just $ case maybeRange of
            Nothing -> Core.fromString (PackageName.print name)
            Just range -> Core.jsonSingletonObject (PackageName.print name) (Core.fromString (Registry.Version.printRange range))
        )
        deps
  decode =
    let
      decodePkgWithRange :: Object Core.Json -> Either String (Tuple PackageName (Maybe Range))
      decodePkgWithRange obj = do
        o <- traverse Yaml.decode obj
        let maybeTuple = Object.toAscUnfoldable o
        case maybeTuple of
          Nothing -> Left "Expected Object here"
          Just (Tuple rawPkg rawRange) -> do
            pkgName <- lmap Parsing.parseErrorMessage $ PackageName.parse rawPkg
            range <- lmap Parsing.parseErrorMessage $ Registry.Version.parseRange Registry.Version.Lenient rawRange
            Right (Tuple pkgName (Just range))

      decodePkg :: String -> Either String (Tuple PackageName (Maybe Range))
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
  , extra_packages :: Maybe (Map PackageName GitPackage)
  }

-- TODO alternateBackend
-- TODO publish config

readConfig :: FilePath -> Aff Config
readConfig path = do
  log "Reading config.."
  eitherConfig :: Either String Config <- liftAff $ Yaml.readYamlFile path
  case eitherConfig of
    Left err -> crash $ "Can't read config: " <> err -- TODO: better error here
    Right config -> do
      log "Read config:"
      -- log (Yaml.printYaml config)
      pure config
