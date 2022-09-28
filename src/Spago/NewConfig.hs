module Spago.NewConfig where

import Spago.Prelude
import Spago.Env

import qualified Data.Yaml
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
  , version :: Text
  , license :: Text
  , dependencies :: Set PackageName
  } deriving (Generic)

instance ToJSON PackageConfig where
  toJSON = genericToJSON customOptions

data WorkspaceConfig = WorkspaceConfig
  { set :: PackageSetAddress
  , backend :: Maybe Text
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
  logInfo (display packageSetUrl)
  -- Get the config, and the fields that we want
  Config { alternateBackend, name, dependencies, migrateConfig } <- view (the @Config)
  (license, version) <- case migrateConfig of
    Left _ -> die [ "Please add the following keys to your spago.dhall: license, version"]
    Right (MigrateConfig {..}) -> pure (migrateLicense, migrateVersion)
  let workspace = WorkspaceConfig { set = PackageSetAddress packageSetUrl, backend = alternateBackend}
  let package = PackageConfig{..}
  let newConfig = NewConfig{..}
  -- Write the new one to YAML
  -- FIXME: we might want to do something about porting these overrides, but it's a lot of work.
  -- One avenue could be to parse the original package set (from the URL that we get), see which packages are different in the one
  -- that Spago reads in, and then stick the differing ones in extra_packages
  logWarn "Any overrides you added to the packages.dhall file were not ported - please add them again to the new configuration"
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
