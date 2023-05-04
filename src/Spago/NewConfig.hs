module Spago.NewConfig where

import Spago.Prelude
import Spago.Env

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import qualified Spago.PackageSet
import qualified Spago.Dhall as Dhall
import qualified Spago.Messages as Messages
import Spago.Dhall (Directory (..), File (..), Scheme (..), URL (..))
import qualified Network.URI.Encode as URI.Encode

data NewConfig = NewConfig
  { package :: PackageConfig
  , workspace :: WorkspaceConfig
  }

instance ToJSON NewConfig where
  toJSON NewConfig{..} = object ["workspace" .= workspace, "package" .= package]
  toEncoding NewConfig{..} = pairs ("package" .= package <> "workspace" .= workspace)

data PackageConfig = PackageConfig
  { name :: Text
  , publish :: Maybe NewPublishConfig
  , dependencies :: Set PackageName
  } deriving (Generic)

instance ToJSON PackageConfig where
  toJSON = genericToJSON customOptions

data NewPublishConfig = NewPublishConfig
  { version :: Maybe Text
  , license :: Maybe Text
  } deriving (Generic)

instance ToJSON NewPublishConfig where
  toJSON = genericToJSON customOptions

data ExtraPackage = ExtraPackage
  { git :: Text
  , ref :: Text
  , dependencies :: Set PackageName
  } deriving (Generic)

instance ToJSON ExtraPackage

data BackendConfig = BackendConfig { cmd :: Text }
  deriving (Generic)

instance ToJSON BackendConfig

data WorkspaceConfig = WorkspaceConfig
  { package_set :: PackageSetAddress
  , backend :: Maybe BackendConfig
  , extra_packages :: Map PackageName ExtraPackage
  } deriving (Generic)

instance ToJSON WorkspaceConfig where
  toJSON = genericToJSON customOptions

newtype PackageSetAddress = PackageSetAddress Text

instance ToJSON PackageSetAddress where
  toJSON (PackageSetAddress url) = object ["url" .= url]

customOptions :: Options
customOptions = defaultOptions
  { omitNothingFields = True
  }

toExtraPackage :: Package -> Maybe ExtraPackage
toExtraPackage Package { dependencies, location } = case location of
  Local {} -> Nothing
  Remote { repo, version } -> Just (ExtraPackage { git = unRepo repo, ref = version, dependencies = dependencies })

migrate :: (HasLogFunc env, HasConfig env) => RIO env ()
migrate = do
  -- Fish out the package set url from the packages.dhall
  rawPackageSet <- liftIO $ Dhall.readRawExpr Spago.PackageSet.packagesPath
  packageSetUrl <- case rawPackageSet of
        Nothing -> die [ display Messages.cannotFindPackages ]
        Just (_, expr)
          | (current:_) <- foldMap newPackageSetUrl expr
          -> pure (renderUrl current)
        Just _ -> die [ display Messages.cannotFindPackageImport ]
  logDebug $ "New package-set URL: " <> display packageSetUrl
  -- Now we _fetch_ this package set, so that we know which packages are there
  logDebug "Fetching the package set.."
  request <- Http.parseRequest $ Text.unpack packageSetUrl
  PackageSet { packagesDB = remotePackagesDB } <- Http.responseBody <$> Http.httpJSON request
  -- Then we get the current package set as read by spago - this includes overrides
  logDebug "Reading the local package set.."
  PackageSet { packagesDB } <- view (the @PackageSet)
  -- Now we can run the difference between them, and add it as extra_packages in the new config
  -- Note: we don't just look for things that are not in the remote set, also things that are different!
  let extra_packages = Map.mapMaybe toExtraPackage $ Map.differenceWith (\p r -> if p == r then Nothing else Just p) packagesDB remotePackagesDB
  -- Get the config, and the fields that we want
  Config { alternateBackend, name, dependencies, migrateConfig } <- view (the @Config)
  let publish = case migrateConfig of
        Left _ -> Nothing
        Right (MigrateConfig {..}) -> Just $ NewPublishConfig { version = Just migrateVersion, license = Just migrateLicense }
  let backend = fmap (\cmd -> BackendConfig{..}) alternateBackend
  -- TODO: we probably want a flag that says "nevermind my package set, give me the latest set from the registry"
  let workspace = WorkspaceConfig { package_set = PackageSetAddress packageSetUrl, backend = backend, extra_packages = extra_packages }
  let package = PackageConfig{..}
  let newConfig = NewConfig{..}
  -- Write the new one to YAML
  logInfo "Writing the new config format to spago.yaml..."
  liftIO $ Data.Yaml.encodeFile "spago.yaml" newConfig

  where
    -- Need to go from e.g. https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220921/packages.dhall
    -- to: https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.4-20220725/packages.json
    newPackageSetUrl :: Dhall.Import -> [Dhall.URL]
    newPackageSetUrl (Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          { authority = "github.com"
          , path = Dhall.File
            { file = "packages.dhall"
            , directory = Dhall.Directory
              { components = [ currentTag, "download", "releases", repo, org ]}
            }
          , ..
          }
        }
      }) =
      let components = [ currentTag, repo, org ]
          directory = Dhall.Directory{..}
          authority = "raw.githubusercontent.com"
          newPath = Dhall.File{ file = "packages.json", .. }
      in [Dhall.URL { path = newPath, ..}]
    newPackageSetUrl _ = []


-------------- From Dhall.URL

renderUrl :: URL -> Text
renderUrl url =
        schemeText
    <>  authority
    <>  pathText
    <>  queryText
  where
    URL {..} = url

    File {..} = path

    Directory {..} = directory

    schemeText = case scheme of
        HTTP  -> "http://"
        HTTPS -> "https://"

    pathText =
            foldMap renderComponent (reverse components)
        <>  renderComponent file

    queryText = foldMap renderQuery query

    renderComponent :: Text -> Text
    renderComponent component = "/" <> URI.Encode.encodeText component

    renderQuery :: Text -> Text
    renderQuery query' = "?" <> query'
