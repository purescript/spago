module Spago.Command.Ls 
  ( listPackageSet
  , listPackages
  ) where

import Spago.Prelude
import Spago.Env

import qualified Data.Aeson               as Json
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import qualified Spago.Packages as Packages


data JsonPackageOutput = JsonPackageOutput
  { json_packageName :: !Text
  , json_repo        :: !Value
  , json_version     :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON JsonPackageOutput where
  toJSON = Json.genericToJSON Json.defaultOptions
    { fieldLabelModifier = drop 5
    }

encodeJsonPackageOutput :: JsonPackageOutput -> Text
encodeJsonPackageOutput = LT.toStrict . LT.decodeUtf8 . Json.encode


listPackageSet
  :: (HasLogFunc env, HasPackageSet env)
  => JsonFlag -> RIO env ()
listPackageSet jsonFlag = do
  logDebug "Running `listPackageSet`"
  PackageSet{..} <- view (the @PackageSet)
  traverse_ output $ formatPackageNames jsonFlag (Map.toList packagesDB)

listPackages
  :: (HasLogFunc env, HasConfig env, HasTarget env, HasTargetName env)
  => IncludeTransitive
  -> JsonFlag
  -> RIO env ()
listPackages packagesFilter jsonFlag = do
  logDebug "Running `listPackages`"
  tgtName <- view (the @TargetName)
  packagesToList :: [(PackageName, Package)] <- case packagesFilter of
    IncludeTransitive -> Packages.getTransitiveTargetDeps
    _ -> do
      Config { packageSet = PackageSet{ packagesDB } } <- view (the @Config)
      Target { targetDependencies } <- view (the @Target)
      pure $ Map.toList $ Map.restrictKeys packagesDB (Set.fromList targetDependencies)

  case packagesToList of
    [] -> logWarn $ "There are no dependencies listed in your spago.dhall for the target '" <> display (targetName tgtName) <> "'"
    _  -> traverse_ output $ formatPackageNames jsonFlag packagesToList

formatPackageNames :: JsonFlag -> [(PackageName, Package)] -> [Text]
formatPackageNames = \case
  JsonOutputYes -> formatPackageNamesJson
  JsonOutputNo  -> formatPackageNamesText
  where
    -- | Format all the packages from the config in JSON
    formatPackageNamesJson :: [(PackageName, Package)] -> [Text]
    formatPackageNamesJson pkgs =
      let
        asJson (PackageName{..}, Package{ location = loc@Remote{..} })
          = JsonPackageOutput
              { json_packageName = packageName
              , json_repo = toJSON loc
              , json_version = version
              }
        asJson (PackageName{..}, Package { location = loc@(Local _) })
          = JsonPackageOutput
              { json_packageName = packageName
              , json_repo = toJSON loc
              , json_version = "local"
              }
      in map (encodeJsonPackageOutput . asJson) pkgs

    -- | Format all the package names from the configuration
    formatPackageNamesText :: [(PackageName, Package)] -> [Text]
    formatPackageNamesText pkgs =
      let
        showVersion Remote{..} = version
        showVersion _                     = "local"

        showLocation Remote{ repo = Repo repo } = "Remote " <> surroundQuote repo
        showLocation Local{..}                  = "Local " <> surroundQuote localPath

        longestName = maximum $ fmap (Text.length . packageName . fst) pkgs
        longestVersion = maximum $ fmap (Text.length . showVersion . location . snd) pkgs

        renderPkg (PackageName{..}, Package{..})
          = leftPad longestName packageName <> " "
          <> leftPad longestVersion (showVersion location) <> "   "
          <> showLocation location
      in map renderPkg pkgs

    leftPad :: Int -> Text -> Text
    leftPad n s
      | Text.length s < n  = s <> Text.replicate (n - Text.length s) " "
      | otherwise = s
