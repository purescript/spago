{-# LANGUAGE OverloadedLists #-}
module Spago.PackageSet
  ( updatePackageSetVersion
  , getLatestSetForCompilerVersion
  , checkPursIsUpToDate
  , makePackageSetFile
  , freeze
  , ensureFrozen
  , packagesPath
  , findRootOutputPath
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Control.Exception as Exception
import           Data.Dynamic (fromDynamic)
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import qualified Data.Versions   as Version
import qualified Dhall.Freeze
import qualified Dhall.Pretty
import           Dhall.Import (PrettyHttpException(..))
import qualified Safe
import qualified System.FilePath
import qualified System.IO

import qualified Spago.Dhall     as Dhall
import qualified Spago.GitHub    as GitHub
import qualified Spago.Messages  as Messages
import qualified Spago.Templates as Templates
import qualified Spago.Purs      as Purs

import           Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus, )
import           Network.HTTP.Types.Status (status404)


packagesPath :: IsString t => t
packagesPath = "packages.dhall"


-- | Tries to create the `packages.dhall` file if needed
makePackageSetFile :: HasLogFunc env => Force -> Dhall.TemplateComments -> RIO env ()
makePackageSetFile force comments = do
  hasPackagesDhall <- testfile packagesPath
  if force == Force || not hasPackagesDhall
    then writeTextFile packagesPath $ Dhall.processComments comments Templates.packagesDhall
    else logWarn $ display $ Messages.foundExistingProject packagesPath
  Dhall.format packagesPath

getLatestSetForCompilerVersion
  :: HasLogFunc env
  => Version.SemVer
  -> Text
  -> Text
  -> RIO env (Either SomeException Text)
getLatestSetForCompilerVersion compilerVersion org repo = do
  logDebug $ "Got a compiler version: " <> displayShow compilerVersion
  -- we first try getting the file with the index of the latest sets
  eitherLatestReleases <- try $ GitHub.getLatestReleasesFile org repo
  logDebug $ "Latest releases: " <> displayShow eitherLatestReleases
  for eitherLatestReleases $ \latestReleases -> do
    -- if we get that, we just try to pick a set for the current compiler
    case Map.lookup compilerVersion latestReleases of
      -- but if the current compiler is not there we check that it's compatible with the latest (see issue #777)
      Nothing ->
        let releasesList = List.reverse $ Map.toList latestReleases
        in case releasesList of
          ((latestVersion,latestSet):_) | isVersionCompatible compilerVersion latestVersion -> pure latestSet
          -- otherwise error explaining the non-compatibility
          _ -> die [ display $ Messages.incompatiblePurs (Version.prettySemVer <$> List.reverse (Map.keys latestReleases))]
      Just newTag -> do
        logDebug $ "Found a new tag in the releases file: " <> display newTag
        pure newTag

-- | Use the specified version of the package set (if specified).
--   Otherwise, get the latest version of the package set if possible
updatePackageSetVersion
  :: forall env
  .  (HasLogFunc env, HasGlobalCache env, HasPurs env)
  => Maybe Text
  -> RIO env ()
updatePackageSetVersion maybeTag = do
  logDebug "Running `spago upgrade-set`"

  rawPackageSet <- liftIO $ Dhall.readRawExpr packagesPath
  (org, repo, currentTag) <- case rawPackageSet of
        Nothing -> die [ display Messages.cannotFindPackages ]
        Just (_, expr)
          | (current:_) <- foldMap getCurrentTag expr
          -> pure current
        Just _ -> die [ display Messages.cannotFindPackageImport ]

  maybe
    (useLatestRelease org repo currentTag)
    (updateTag org repo currentTag)
    maybeTag
  where
    -- | Try to fetch the latest compatible tag in various ways, and updates
    --   the packages.dhall with it
    useLatestRelease
      :: Text -> Text -> Text -> RIO env ()
    useLatestRelease org repo currentTag = do
      let getLatestFromGitHub = GitHub.getLatestPackageSetsTag org repo >>= \case
            Right tag -> updateTag org repo currentTag tag
            Left (err :: SomeException) -> do
              logWarn "Was not possible to upgrade the package-sets release"
              logDebug $ "Error: " <> display err
      PursCmd { compilerVersion } <- view (the @PursCmd)
      getLatestSetForCompilerVersion compilerVersion org repo >>= \case
        Right newTag -> updateTag org repo currentTag newTag
        -- if that fails then we fetch the latest set from GitHub
        -- TODO: check that it's compatible with the current compiler
        Left _err -> getLatestFromGitHub

    -- | Tries to upgrade the package-sets release of the local package set.
    --   It will:
    --   - try to read the current package-set file
    --   - try to replace the git tag to which the package-set imports point to
    --   - if all of this succeeds, it will regenerate the hashes and write to file
    updateTag :: Text -> Text -> Text -> Text -> RIO env ()
    updateTag org repo currentTag specificTag =  do
      let quotedTag = surroundQuote specificTag
          orgRepo = org <> "/" <> repo
      logDebug $ "Attempting to use tag for " <> display (surroundQuote orgRepo) <> ": " <> display quotedTag
      rawPackageSet <- liftIO $ Dhall.readRawExpr packagesPath
      case rawPackageSet of
        Nothing -> die [ display Messages.cannotFindPackages ]
        -- Skip the check if the tag is already being used
        Just _
          | currentTag == specificTag
            -> logDebug $ "Skipping package set version update, already on version: " <> display quotedTag
        Just (header, expr) -> do
          let newExpr = fmap (upgradeImports org repo specificTag) expr
          logInfo $ display $ Messages.updatingPackageSet specificTag
          try (liftIO $ Dhall.writeRawExpr packagesPath (header, newExpr)) >>= \case
            Right _ -> do
              -- If everything is fine, refreeze the imports
              freeze packagesPath
            Left e@(PrettyHttpException _ httpError) -> do
              case fromDynamic httpError of
                Just (HttpExceptionRequest _ (StatusCodeException resp _))
                  | responseStatus resp == status404 -> do
                      -- If Dhall got a 404 when checking remote, warn user
                      logWarn $ display $ Messages.nonExistentPackageSet org repo currentTag specificTag
                _ -> do
                  liftIO $ Exception.throwIO e

    getCurrentTag :: Dhall.Import -> [(Text, Text, Text)]
    getCurrentTag Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          -- Check if we're dealing with the right repo
          { authority = "github.com"
          , path = Dhall.File
            { file = "packages.dhall"
            , directory = Dhall.Directory
              { components = [ currentTag, "download", "releases", repo, org ]}
            }
          }
        }
      } = [(org, repo, currentTag)]
    getCurrentTag _ = []

    -- | Given an import and a new package-sets tag,
    --   upgrades the import to the tag and resets the hash
    upgradeImports :: Text -> Text -> Text -> Dhall.Import -> Dhall.Import
    upgradeImports org repo newTag (Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          { authority = "github.com"
          , path = Dhall.File
            { file = "packages.dhall"
            , directory = Dhall.Directory
              { components = [ _currentTag, "download", "releases", upgradeRepo, upgradeOrg ]}
            }
          , ..
          }
        }
      , ..
      }) | upgradeRepo == repo && upgradeOrg == org =
      let components = [ newTag, "download", "releases", repo, org ]
          directory = Dhall.Directory{..}
          newPath = Dhall.File{ file = "packages.dhall", .. }
          authority = "github.com"
          importType = Dhall.Remote Dhall.URL { path = newPath, ..}
          newHash = Nothing -- Reset the hash here, as we'll refreeze
          importHashed = Dhall.ImportHashed { hash = newHash, ..}
      in Dhall.Import{..}
    upgradeImports _ _ _ imp = imp


checkPursIsUpToDate :: forall env. (HasLogFunc env, HasPackageSet env) => RIO env ()
checkPursIsUpToDate = do
  logDebug "Checking if `purs` is up to date"
  PackageSet{..} <- view (the @PackageSet)
  eitherCompilerVersion <- Purs.pursVersion
  case (eitherCompilerVersion, packagesMinPursVersion) of
    (Right compilerVersion, Just pursVersionFromPackageSet) ->
      unless (isVersionCompatible compilerVersion pursVersionFromPackageSet) $ do
        die [ display
              $ Messages.pursVersionMismatch
              (Version.prettySemVer compilerVersion)
              (Version.prettySemVer pursVersionFromPackageSet) ]
    (compilerVersion, packageSetVersion) -> do
      logWarn "Unable to parse compiler and package set versions, not checking if `purs` is compatible with it.."
      logDebug "Versions we got:"
      logDebug $ " - from the compiler: " <> displayShow compilerVersion
      logDebug $ " - in package set: " <> displayShow packageSetVersion

-- | The check is successful only when the installed compiler is "slightly"
--   greater or equal to the minimum version. E.g. fine cases are:
--   - current is 0.12.2 and package-set is on 0.12.1
--   - current is 1.4.3 and package-set is on 1.3.4
--   Not fine cases are e.g.:
--   - current is 0.1.2 and package-set is 0.2.3
--   - current is 1.2.3 and package-set is 1.3.4
--   - current is 1.2.3 and package-set is 0.2.3
isVersionCompatible :: Version.SemVer -> Version.SemVer -> Bool
isVersionCompatible installedVersion minVersion =
  let versionList semver = semver ^.. (Version.major <> Version.minor <> Version.patch)
  in case (versionList installedVersion, versionList minVersion) of
    ([0, b, c], [0, y, z]) | b == y && c >= z -> True
    ([a, b, _c], [x, y, _z]) | a /= 0 && a == x && b >= y -> True
    _ -> False

isRemoteFrozen :: Dhall.Import -> [Bool]
isRemoteFrozen (Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = Dhall.Remote _
    , hash
    }
  })             = [isJust hash]
isRemoteFrozen _ = []


localImportPath :: Dhall.Import -> Maybe System.IO.FilePath
localImportPath (Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = localImport@(Dhall.Local _ _)
    }
  })              = Just $ Text.unpack $ pretty localImport
localImportPath _ = Nothing


rootPackagePath :: Dhall.Import -> Maybe System.IO.FilePath
rootPackagePath (Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = localImport@(Dhall.Local _ Dhall.File { file = "packages.dhall" })
    }
  , Dhall.importMode = Dhall.Code
  })              = Just $ Text.unpack $ pretty localImport
rootPackagePath _ = Nothing


-- | In a Monorepo we don't wish to rebuild our shared packages over and over,
-- | so we build into an output folder where our root packages.dhall lives
findRootOutputPath :: HasLogFunc env => System.IO.FilePath -> RIO env (Maybe System.IO.FilePath)
findRootOutputPath path = do
  logDebug "Locating root path of packages.dhall"
  imports <- liftIO $ Dhall.readImports $ Text.pack path
  let localImports = mapMaybe rootPackagePath imports
  pure $ flip System.FilePath.replaceFileName "output" <$> findRootPath localImports

-- | Given a list of filepaths, find the one with the least folders
findRootPath :: [System.IO.FilePath] -> Maybe System.IO.FilePath
findRootPath = Safe.minimumByMay (comparing (length . System.FilePath.splitSearchPath))

-- | Freeze the package set remote imports so they will be cached
freeze :: HasLogFunc env => System.IO.FilePath -> RIO env ()
freeze path = do
  logInfo $ display Messages.freezePackageSet
  liftIO $
    Dhall.Freeze.freeze
      Dhall.Write
      Dhall.NonTransitive
      [Dhall.InputFile path]
      Dhall.Freeze.OnlyRemoteImports
      Dhall.Freeze.Secure
      (Just Dhall.Pretty.ASCII)
      Dhall.NoCensor


-- | Freeze the file if any of the remote imports are not frozen
ensureFrozen :: HasLogFunc env => System.IO.FilePath -> RIO env ()
ensureFrozen path = do
  logDebug "Ensuring that the package set is frozen"
  imports <- liftIO $ Dhall.readImports $ Text.pack path
  let areRemotesFrozen = foldMap isRemoteFrozen imports
  case areRemotesFrozen of
    []      -> logWarn $ display $ Messages.failedToCheckPackageSetFrozen
    remotes -> unless (and remotes) $
      traverse_ (maybe (pure ()) freeze . localImportPath) imports
