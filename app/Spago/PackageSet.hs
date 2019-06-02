module Spago.PackageSet
  ( upgradePackageSet
  , checkPursIsUpToDate
  , makePackageSetFile
  , freeze
  , ensureFrozen
  , path
  , pathText
  , PackageSet
  , Package (..)
  , PackageName (..)
  , Repo (..)
  ) where

import           Spago.Prelude

import qualified Data.Text       as Text
import qualified Data.Versions   as Version
import qualified Dhall
import           Dhall.Binary    (defaultStandardVersion)
import qualified Dhall.Freeze
import qualified Dhall.Pretty
import qualified GitHub
import           Network.URI     (parseURI)

import qualified Spago.Dhall     as Dhall
import           Spago.Messages  as Messages
import qualified Spago.Purs      as Purs
import qualified Spago.Templates as Templates


newtype PackageName = PackageName { packageName :: Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.Interpret)

-- | A package-set package.
--   Matches the packages definition in Package.dhall from package-sets
data Package = Package
  { dependencies :: ![PackageName] -- ^ list of dependency package names
  , repo         :: !Repo          -- ^ the remote git repository or the local path
  , version      :: !Text          -- ^ version string (also functions as a git ref)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Package
instance FromJSON Package

type PackageSet = Map PackageName Package

-- | We consider a "Repo" a "box of source to include in the build"
--   This can have different nature:
data Repo
  = Local !Text    -- ^ A local path
  | Remote !Text   -- ^ The address of a remote git repository
  deriving (Eq, Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

instance Dhall.Interpret Repo where
  autoWith _ = makeRepo <$> Dhall.strictText
    where
      -- We consider a "Remote" anything that `parseURI` thinks is a URI
      makeRepo repo = case parseURI $ Text.unpack repo of
        Just _uri -> Remote repo
        Nothing   -> Local repo


pathText :: Text
pathText = "packages.dhall"

path :: FilePath
path = pathFromText pathText


-- | Tries to create the `packages.dhall` file. Fails when the file already exists,
--   unless `--force` has been used.
makePackageSetFile :: Bool -> IO ()
makePackageSetFile force = do
  unless force $ do
    hasPackagesDhall <- testfile path
    when hasPackagesDhall $ echo $ Messages.foundExistingProject pathText
  writeTextFile path Templates.packagesDhall
  Dhall.format pathText


-- | Tries to upgrade the Package-Sets release of the local package set.
--   It will:
--   - try to read the latest tag from GitHub
--   - try to read the current package-set file
--   - try to replace the git tag to which the package-set imports point to
--     (if they point to the Package-Sets repo. This can be eventually made GitHub generic)
--   - if all of this succeeds, it will regenerate the hashes and write to file
upgradePackageSet :: Spago m => m ()
upgradePackageSet = do
  result <- liftIO $ GitHub.executeRequest' $ GitHub.latestReleaseR "purescript" "package-sets"
  case result of
    Left err -> die $ Messages.failedToReachGitHub err
    Right GitHub.Release{..} -> do
      echo ("Found the most recent tag for \"purescript/package-sets\": " <> surroundQuote releaseTagName)
      rawPackageSet <- Dhall.readRawExpr pathText
      case rawPackageSet of
        Nothing -> die Messages.cannotFindPackages
        Just (header, expr) -> do
          let newExpr = fmap (upgradeImports releaseTagName) expr
          echo $ Messages.upgradingPackageSet releaseTagName
          Dhall.writeRawExpr pathText (header, newExpr)
          -- If everything is fine, refreeze the imports
          freeze
  where
    -- | Given an import and a new purescript/package-sets tag,
    --   upgrades the import to the tag and resets the hash
    upgradeImports :: Text -> Dhall.Import -> Dhall.Import
    upgradeImports newTag imp@(Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          -- Check if we're dealing with the right repo
          { authority = "raw.githubusercontent.com"
          , path = Dhall.File
            { directory = Dhall.Directory
              { components = [ "src", _currentTag, ghRepo, ghOrg ]}
            , ..
            }
          , ..
          }
        , ..
        }
      , ..
      }) =
      let components = [ "src", newTag, "package-sets", "purescript" ]
          directory = Dhall.Directory{..}
          newPath = Dhall.File{..}
          authority = "raw.githubusercontent.com"
          importType = Dhall.Remote Dhall.URL { path = newPath, ..}
          newHash = Nothing -- Reset the hash here, as we'll refreeze
          importHashed = Dhall.ImportHashed { hash = newHash, ..}
          newImport = Dhall.Import{..}
      in case (ghOrg, ghRepo) of
        ("spacchetti", "spacchetti")   -> newImport
        ("purescript", "package-sets") -> newImport
        _                              -> imp
    upgradeImports _ imp = imp


checkPursIsUpToDate :: Spago m => m ()
checkPursIsUpToDate = do
  rawPackageSet <- Dhall.readRawExpr pathText
  case rawPackageSet of
    Nothing -> echo Messages.cannotFindPackagesButItsFine
    Just (_header, expr) -> do
      maybeCompilerVersion <- Purs.version
      let packageSetTags = foldMap getPackageSetTag expr

      -- Let's talk backwards-compatibility.
      -- At some point we switched Spacchetti from tagging the PackageSet with
      -- something like '20180923'  to something like '0.12.2-20190209' instead
      -- (in order to support this check).
      -- Now, if people are still using the old tag, we should:
      -- - warn them to upgrade with `package-set-upgrade`
      -- - skip this check
      --
      -- Update 2019-02-28: we switched from Spacchetti to package-sets, which
      -- uses a different versioning (with "psc-" in front). We just strip it away
      case fmap (Text.split (=='-') . Text.replace "psc-" "") packageSetTags of
        ((minPursVersion:_):_)
          | Just compilerVersion <- maybeCompilerVersion
          , Just pursVersionFromPackageSet <- hush $ Version.semver minPursVersion -> do
          performCheck compilerVersion pursVersionFromPackageSet
        _ -> echo "WARNING: unable to parse compiler and package set versions, skipping check.."

  where
    -- | The check is successful only when the installed compiler is "slightly"
    --   greater (or equal of course) to the minimum version. E.g. fine cases are:
    --   - current is 0.12.2 and package-set is on 0.12.1
    --   - current is 1.4.3 and package-set is on 1.3.4
    --   Not fine cases are e.g.:
    --   - current is 0.1.2 and package-set is 0.2.3
    --   - current is 1.2.3 and package-set is 1.3.4
    --   - current is 1.2.3 and package-set is 0.2.3
    performCheck :: Spago m => Version.SemVer -> Version.SemVer -> m ()
    performCheck actualPursVersion minPursVersion = do
      let versionList semver = semver ^.. (Version.major <> Version.minor <> Version.patch)
      case (versionList actualPursVersion, versionList minPursVersion) of
        ([0, b, c], [0, y, z]) | b == y && c >= z -> pure ()
        ([a, b, _c], [x, y, _z]) | a /= 0 && a == x && b >= y -> pure ()
        _ -> die $ Messages.pursVersionMismatch
            (Version.prettySemVer actualPursVersion)
            (Version.prettySemVer minPursVersion)

    -- | Given a Dhall.Import extract the GitHub tag if it's the upstream import, and
    --   if it points to purescript/package-sets or spacchetti
    getPackageSetTag :: Dhall.Import -> [Text]
    getPackageSetTag (Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          -- Check if we're dealing with the right repo
          { authority = "raw.githubusercontent.com"
          , path = Dhall.File
            { directory = Dhall.Directory
              { components = [ "src", tag, ghRepo, ghOrg ]}
            , file = "packages.dhall"
            }
          , ..
          }
        , ..
        }
      , ..
      }) = case (ghOrg, ghRepo) of
             ("spacchetti", "spacchetti")   -> [tag]
             ("purescript", "package-sets") -> [tag]
             _                              -> []
    getPackageSetTag _ = []


isRemoteFrozen :: Dhall.Import -> [Bool]
isRemoteFrozen (Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = Dhall.Remote _
    , ..
    }
  , ..
  })             = [isJust hash]
isRemoteFrozen _ = []


-- | Freeze the package set remote imports so they will be cached
freeze :: Spago m => m ()
freeze = do
  echo Messages.freezePackageSet
  liftIO $ do
    Dhall.Freeze.freeze (Just $ Text.unpack pathText) False Dhall.Pretty.ASCII defaultStandardVersion
    Dhall.format pathText


-- | Freeze the file if any of the remote imports are not frozen
ensureFrozen :: Spago m => m ()
ensureFrozen = do
  rawPackageSet <- Dhall.readRawExpr pathText
  case rawPackageSet of
    Nothing -> echo "WARNING: wasn't able to check if your package set file is frozen"
    Just (_header, expr) -> do
      let areRemotesFrozen = foldMap isRemoteFrozen expr
      unless (and areRemotesFrozen) $ do
        freeze
