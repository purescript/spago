module Spago.Fetch
  ( fetchPackages
  , getLocalCacheDir
  ) where

import           Spago.Prelude

import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Compression.GZip        as GZip
import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Foldl                 as Fold
import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector
import qualified GitHub
import qualified Network.HTTP.Simple           as Http
import qualified System.Directory              as Directory
import qualified System.Environment
import qualified System.FilePath               as FilePath
import qualified System.IO.Temp                as Temp
import qualified System.Process                as Process
import qualified Turtle

import qualified Spago.Messages                as Messages
import           Spago.PackageSet              (Package (..), PackageName (..), Repo (..))
import qualified Spago.PackageSet              as PackageSet


-- | Algorithm for fetching dependencies:
--   * get in input a list of Packages to possibly fetch
--   * if a Package is local or in the local cache, skip it
--   * Start processing the remaining packages in parallel:
--     * if a Package is in the global cache, copy it to the local cache
--     * then check if the Package is on GitHub and an "immutable" ref:
--       * if yes, download the tar archive and copy it to global and then local cache
--       * if not, run a series of git commands to get the code, and copy to local cache
fetchPackages :: Spago m => Maybe Int -> [(PackageName, Package)] -> m ()
fetchPackages maybeLimit allDeps = do

  PackageSet.checkPursIsUpToDate

  -- Ensure both local and global cache dirs are there
  globalCacheDir >>= assertDirectory
  (pure localCacheDir) >>= assertDirectory

  -- We try to fetch a dep only if their local cache directory doesn't exist
  -- (or their local path, which is the same thing)
  depsToFetch <- (flip filterM) allDeps $ \dep -> do
    exists <- liftIO $ Directory.doesDirectoryExist $ getLocalCacheDir dep
    pure $ not exists

  let nOfDeps = List.length depsToFetch
  when (nOfDeps > 0) $
    echoStr $ "Installing " <> show nOfDeps <> " dependencies."

  -- By default we limit the concurrency to 10
  withTaskGroup' (fromMaybe 10 maybeLimit) $ \taskGroup -> do
    asyncs <- for depsToFetch (async' taskGroup . fetchPackage)
    liftIO $ handle (handler asyncs) (for_ asyncs Async.wait)
    echo "Installation complete."
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
    handler asyncs (e :: SomeException) = do
      for_ asyncs $ \async -> do
        Async.cancel async
        Async.waitCatch async
      die $ "Installation failed.\n\nError:\n\n" <> Messages.tshow e


-- | If the repo points to a remote git, fetch it in the local .spago folder, while
--   eventually caching it to the global cache, or copying it from there if it's
--   sensible to do so.
--   If it's a local directory do nothing
fetchPackage :: Spago m => (PackageName, Package) -> m ()
fetchPackage (PackageName package, Package { repo = Local path }) =
  echo $ Messages.foundLocalPackage package path
fetchPackage pair@(packageName'@PackageName{..}, Package{ repo = Remote repo, ..} ) = do
  globalDir <- globalCacheDir
  let packageDir = getPackageDir packageName' version
      packageGlobalCacheDir = globalDir </> packageDir

  packageLocalCacheDir <- makeAbsolute $ getLocalCacheDir pair

  inGlobalCache <- testdir $ Turtle.decodeString packageGlobalCacheDir
  Temp.withSystemTempDirectory (Text.unpack (packageName <> "-" <> version)) $ \path -> do
    let downloadDir = path </> "download"

    -- * if a Package is in the global cache, copy it to the local cache
    if inGlobalCache
      then do
        echo $ "Copying from global cache: " <> quotedName
        cptree packageGlobalCacheDir downloadDir
        assertDirectory (localCacheDir </> Text.unpack packageName)
        mv downloadDir packageLocalCacheDir
      else do
        -- * otherwise, check if the Package is on GitHub and an "immutable" ref
        -- * if yes, download the tar archive and copy it to global and then local cache
        let cacheableCallback :: Spago m => FilePath.FilePath -> m ()
            cacheableCallback resultDir = do
              -- the idea here is that we first copy the tree in the temp folder,
              -- then atomically move it to the caches
              echo $ "Installing and globally caching " <> quotedName
              let resultDir2 = path </> "download2"
              assertDirectory resultDir2
              cptree resultDir resultDir2
              mv resultDir packageGlobalCacheDir
              mv resultDir2 packageLocalCacheDir

        -- * if not, run a series of git commands to get the code, and move it to local cache
        let nonCacheableCallback :: Spago m => m ()
            nonCacheableCallback = do
              echo $ "Installing " <> quotedName

              -- Here we set the package directory as the cwd of the new process.
              -- This is the "right" way to do it (instead of using e.g.
              -- System.Directory.withCurrentDirectory), as that's apparently
              -- not thread-safe
              let processWithNewCwd = (Process.shell (Text.unpack git))
                    { Process.cwd = Just downloadDir }

              (systemStrictWithErr processWithNewCwd empty) >>= \case
                (ExitSuccess, _, _) -> mv downloadDir packageLocalCacheDir
                (_, _stdout, stderr) -> die $ Messages.failedToInstallDep quotedName stderr

        -- Make sure that the following folders exist first:
        assertDirectory downloadDir
        -- ^ the folder to store the download
        assertDirectory (globalDir </> Text.unpack packageName)
        -- ^ the parent package folder in the global cache (that stores all the versions)
        assertDirectory (localCacheDir </> Text.unpack packageName)
        -- ^ the parent package folder in the local cache (that stores all the versions)

        globallyCache repo version downloadDir cacheableCallback nonCacheableCallback


  where
    quotedName = Messages.surroundQuote packageName

    git = Text.intercalate " && "
           [ "git init"
           , "git remote add origin " <> repo
           , "git fetch origin " <> version
           , "git -c advice.detachedHead=false checkout FETCH_HEAD"
           ]


-- | Fetch the tarball at `archiveUrl` and unpack it into `destination`
fetchTarball :: FilePath.FilePath -> Text -> IO ()
fetchTarball destination archiveUrl = do
  tarballUrl <- Http.parseRequest $ Text.unpack archiveUrl
  lbs <- fmap Http.getResponseBody (Http.httpLBS tarballUrl)
  Tar.unpack destination $ Tar.read $ GZip.decompress lbs


-- | A package is "globally cacheable" if:
--   * it's a GitHub repo
--   * the ref we have is a commit or a tag -- i.e. "immutable enough", so e.g. not a branch
--
--   So here we check that one of the two is true, and if so we run the callback with the
--   URL of the .tar.gz archive on GitHub, otherwise another callback for when it's not
globallyCache :: Spago m => Text -> Text -> FilePath.FilePath -> (FilePath.FilePath -> m ()) -> m () -> m ()
globallyCache url version downloadDir cacheableCallback notCacheableCallback = do
  case (Text.stripPrefix "https://github.com/" url)
       >>= (Text.stripSuffix ".git")
       >>= (Just . Text.split (== '/')) of
    Just [owner, repo] -> do
      try (isTag <|> isCommit) >>= \case
        Left (err :: IOException) -> do
          echoStr $ show err
          notCacheableCallback -- TODO: nice error?
        Right ref -> do
          let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> ref <> ".tar.gz"
          liftIO $ fetchTarball downloadDir archiveUrl
          Just resultDir <-  Turtle.fold (Turtle.ls $ Turtle.decodeString downloadDir) Fold.head
          cacheableCallback $ Turtle.encodeString resultDir
      where
        isTag = do
          res <- liftIO $ GitHub.executeRequest'
                $ GitHub.tagsForR (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo) GitHub.FetchAll
          case res of
            Right tags | Vector.elem version $ fmap GitHub.tagName tags -> return version
            _                                                           -> empty

        isCommit = do
          res <- liftIO $ GitHub.executeRequest'
                $ GitHub.gitCommitR (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo) (GitHub.mkName Proxy version)
          case res of
            Right _commit -> return version
            _             -> empty
    other -> do
      echoStr $ show other
      notCacheableCallback -- TODO: error?


-- | Directory in which spago will put its local cache
localCacheDir :: FilePath.FilePath
localCacheDir = ".spago"


-- | Given a package name and a ref, return a FilePath for the package,
--   to be used as a prefix in local and global cache
getPackageDir :: PackageName -> Text -> FilePath.FilePath
getPackageDir PackageName{..} version = Text.unpack packageName </> Text.unpack version


-- | Returns the path in the local cache for a given package
--   If the package is from a remote git repo, return the folder inside the local cache
--   Otherwise return the local folder
getLocalCacheDir :: (PackageName, Package) -> FilePath.FilePath
getLocalCacheDir (packageName, Package{ repo = Remote _, ..}) = do
  localCacheDir </> getPackageDir packageName version
getLocalCacheDir (_, Package{ repo = Local path }) =
  Text.unpack path


-- | Directory in which spago will put its global cache
-- | Code from: https://github.com/dhall-lang/dhall-haskell/blob/d8f2787745bb9567a4542973f15e807323de4a1a/dhall/src/Dhall/Import.hs#L578
globalCacheDir :: (Alternative m, MonadIO m) => m FilePath.FilePath
globalCacheDir = do
  -- TODO: fail with a nice error in case of empty
  cacheDir <- alternative₀ <|> alternative₁
  pure $ cacheDir </> "spago"
  where
    alternative₀ = do
      maybeXDGCacheHome <- do
        liftIO (System.Environment.lookupEnv "XDG_CACHE_HOME")

      case maybeXDGCacheHome of
        Just xdgCacheHome -> return xdgCacheHome
        Nothing           -> empty

    alternative₁ = do
      maybeHomeDirectory <- liftIO (System.Environment.lookupEnv "HOME")

      case maybeHomeDirectory of
        Just homeDirectory -> return (homeDirectory </> ".cache")
        Nothing            -> empty


-- | Code from: https://github.com/dhall-lang/dhall-haskell/blob/d8f2787745bb9567a4542973f15e807323de4a1a/dhall/src/Dhall/Import.hs#L578
assertDirectory :: (MonadIO m, Alternative m) => FilePath.FilePath -> m ()
assertDirectory directory = do
  let private = transform Directory.emptyPermissions
        where
          transform =
            Directory.setOwnerReadable   True
            .   Directory.setOwnerWritable   True
            .   Directory.setOwnerSearchable True

  let accessible path =
        Directory.readable   path
        && Directory.writable   path
        && Directory.searchable path

  directoryExists <- liftIO (Directory.doesDirectoryExist directory)

  if directoryExists
    then do
      permissions <- liftIO (Directory.getPermissions directory)

      guard (accessible permissions)

    else do
      assertDirectory (FilePath.takeDirectory directory)

      liftIO (Directory.createDirectory directory)

      liftIO (Directory.setPermissions directory private)
