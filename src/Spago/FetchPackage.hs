module Spago.FetchPackage
  ( fetchPackages
  , getLocalCacheDir
  , getCacheVersionDir
  ) where

import           Spago.Prelude

import qualified Control.Concurrent.Async.Pool as Async
import qualified Data.ByteString               as ByteString
import qualified Data.Char                     as Char
import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Versions                 as Version
import qualified Numeric
import qualified System.FilePath               as FilePath
import qualified System.IO.Temp                as Temp
import qualified System.Process                as Process
import qualified Turtle
import qualified UnliftIO.Directory            as Directory

import qualified Spago.GlobalCache             as GlobalCache
import qualified Spago.Messages                as Messages
import qualified Spago.PackageSet              as PackageSet

import           Spago.Types


-- | Algorithm for fetching dependencies:
--   * get in input a list of Packages to possibly fetch
--   * if a Package is local or in the local cache, skip it
--   * Start processing the remaining packages in parallel:
--     * if a Package is in the global cache, copy it to the local cache
--     * then check if the Package is on GitHub and an "immutable" ref:
--       * if yes, download the tar archive and copy it to global and then local cache
--       * if not, run a series of git commands to get the code, and copy to local cache
fetchPackages
  :: Maybe GlobalCache.CacheFlag
  -> [(PackageName, Package)]
  -> Maybe Version.SemVer
  -> Spago ()
fetchPackages globalCacheFlag allDeps minPursVersion = do
  logDebug "Running `fetchPackages`"

  PackageSet.checkPursIsUpToDate minPursVersion

  -- Ensure both local and global cache dirs are there
  globalCache <- askEnv envGlobalCache
  assertDirectory globalCache
  assertDirectory localCacheDir

  -- We try to fetch a dep only if their local cache directory doesn't exist
  -- (or their local path, which is the same thing)
  depsToFetch <- flip filterM allDeps $ \dep -> do
    exists <- Directory.doesDirectoryExist $ getLocalCacheDir dep
    pure $ not exists

  -- If we have to actually fetch any package, we get the Github Index
  -- Note: it might be empty depending on the cacheFlag
  let nOfDeps = List.length depsToFetch
  when (nOfDeps > 0) $ do
    logInfo $ "Installing " <> display nOfDeps <> " dependencies."
    metadata <- GlobalCache.getMetadata globalCacheFlag

    limit <- askEnv envJobs
    withTaskGroup' limit $ \taskGroup -> do
      asyncs <- for depsToFetch (async' taskGroup . fetchPackage metadata)
      handle (handler asyncs) (for_ asyncs wait')

  logInfo "Installation complete."

  where
    -- Here we have this weird exception handling so that threads can clean after
    -- themselves (e.g. remove the directory they might have created) in case an
    -- asynchronous exception happens.
    -- So if any Exception happens while `wait`ing for any thread, we go over all
    -- the `asyncs` (the completed ones will not be affected) and `cancel` them.
    -- This throws an AsyncException in their thread, which causes the bracket to
    -- run the cleanup. However, we have to be careful afterwards, as `cancel` only
    -- waits for the exception to be thrown there, and we have to `wait` ourselves
    -- (with `waitCatch` so that we ignore any exception we are thrown and the `for_`
    -- completes) for the asyncs to finish their cleanup.
    handler :: (HasLogFunc env, MonadReader env m, MonadIO m) => [Async.Async ()] -> SomeException -> m ()
    handler asyncs (e :: SomeException) = do
      for_ asyncs $ \asyncTask -> do
        cancel' asyncTask
        waitCatch' asyncTask
      die [ "Installation failed", "Error:", display e ]


-- | If the repo points to a remote git, fetch it in the local .spago folder, while
--   eventually caching it to the global cache, or copying it from there if it's
--   sensible to do so.
--   If it's a local directory do nothing
fetchPackage :: GlobalCache.ReposMetadataV1 -> (PackageName, Package) -> Spago ()
fetchPackage _ (PackageName package, Package { location = Local{..}, .. }) =
  output $ Messages.foundLocalPackage package localPath
fetchPackage metadata pair@(packageName'@PackageName{..}, Package{ location = Remote{..}, .. } ) = do
  logDebug $ "Fetching package " <> display packageName
  globalCache <- askEnv envGlobalCache
  let packageDir = getPackageDir packageName' version
      packageGlobalCacheDir = globalCache </> packageDir

  packageLocalCacheDir <- makeAbsolute $ getLocalCacheDir pair

  inGlobalCache <- testdir $ Turtle.decodeString packageGlobalCacheDir
  Temp.withTempDirectory localCacheDir (Text.unpack ("__download-" <> packageName <> "-" <> getCacheVersionDir version)) $ \path -> do
    let downloadDir = path </> "download"

    -- * if a Package is in the global cache, copy it to the local cache
    if inGlobalCache
      then do
        output $ "Copying from global cache: " <> quotedName
        cptree packageGlobalCacheDir downloadDir
        assertDirectory (localCacheDir </> Text.unpack packageName)
        mv downloadDir packageLocalCacheDir
      else Temp.withTempDirectory globalCache (Text.unpack ("__temp-" <> "-" <> packageName <> getCacheVersionDir version)) $ \globalTemp -> do
        -- * otherwise, check if the Package is on GitHub and an "immutable" ref
        -- * if yes, download the tar archive and copy it to global and then local cache
        let cacheableCallback :: FilePath.FilePath -> Spago ()
            cacheableCallback resultDir = do
              -- the idea here is that we first copy the tree to a temp folder,
              -- then atomically move it to the correct cache location.  Since
              -- `mv` will not move folders across filesystems, this temp
              -- is created inside globalDir, guaranteeing the same filesystem.
              output $ "Installing and globally caching " <> quotedName
              let resultDir2 = globalTemp </> "download2"
              assertDirectory resultDir2
              cptree resultDir resultDir2
              catch (mv resultDir2 packageGlobalCacheDir) $ \(err :: SomeException) ->
                output $ Messages.failedToCopyToGlobalCache err
              mv resultDir packageLocalCacheDir

        -- * if not, run a series of git commands to get the code, and move it to local cache
        let nonCacheableCallback :: Spago ()
            nonCacheableCallback = do
              output $ "Installing " <> quotedName

              -- Here we set the package directory as the cwd of the new process.
              -- This is the "right" way to do it (instead of using e.g.
              -- System.Directory.withCurrentDirectory), as that's apparently
              -- not thread-safe
              let processWithNewCwd = (Process.shell (Text.unpack git))
                    { Process.cwd = Just downloadDir }

              systemStrictWithErr processWithNewCwd empty >>= \case
                (ExitSuccess, _, _) -> mv downloadDir packageLocalCacheDir
                (_, _out, err) -> die [ display $ Messages.failedToInstallDep quotedName err ]

        -- Make sure that the following folders exist first:
        assertDirectory downloadDir
        -- ^ the folder to store the download
        assertDirectory (globalCache </> Text.unpack packageName)
        -- ^ the parent package folder in the global cache (that stores all the versions)
        assertDirectory (localCacheDir </> Text.unpack packageName)
        -- ^ the parent package folder in the local cache (that stores all the versions)

        GlobalCache.globallyCache
          (packageName', repo, version)
          downloadDir
          metadata
          cacheableCallback
          nonCacheableCallback

  where
    quotedName = surroundQuote packageName

    git = Text.intercalate " && "
           [ "git clone " <> unRepo repo <> " ."
           , "git -c advice.detachedHead=false checkout " <> version
           ]


-- | Directory in which spago will put its local cache
localCacheDir :: FilePath.FilePath
localCacheDir = ".spago"


-- | Given a package name and a ref, return a FilePath for the package,
--   to be used as a prefix in local and global cache
getPackageDir :: PackageName -> Text -> FilePath.FilePath
getPackageDir PackageName{..} version = Text.unpack packageName <> "/" <> Text.unpack (getCacheVersionDir version)


-- | Returns the path in the local cache for a given package
--   If the package is from a remote git repo, return the folder inside the local cache
--   Otherwise return the local folder
getLocalCacheDir :: (PackageName, Package) -> FilePath.FilePath
getLocalCacheDir (packageName, Package{ location = Remote{..}, .. }) = do
  localCacheDir <> "/" <> getPackageDir packageName version
getLocalCacheDir (_, Package{ location = Local{..}, .. }) =
  Text.unpack localPath


-- | Returns the name of the cache dir based on the ref, escaped if necessary.
-- This function must be injective and must always produce valid directory
-- names, which means that problematic characters like / or : will be escaped
-- using a scheme similar to URL-encoding. Note in particular that the function
-- must be injective in a case-insensitive manner if we want this to work
-- reliably on case-insensitive filesystems, in the sense that two different
-- inputs must map to two different outputs _and_ those outputs must differ by
-- more than just casing.
--
-- The characters which are most commonly used in version and branch names are
-- those which we allow through as they are (without escaping).
getCacheVersionDir :: Text -> Text
getCacheVersionDir = Text.concatMap replace
  where
    escape = Text.pack . foldMap ((<>) "%" . flip Numeric.showHex "") . ByteString.unpack . Text.encodeUtf8
    replace c = if Char.isLower c || Char.isDigit c || c `elem` ['.', ',', '-', '_', '+']
      then Text.singleton c
      else escape (Text.singleton c)
