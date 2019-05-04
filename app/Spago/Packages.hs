module Spago.Packages
  ( initProject
  , install
  , sources
  , verify
  , listPackages
  , getGlobs
  , getProjectDeps
  , fetchPackages
  , PackageSet.upgradePackageSet
  , PackageSet.freeze
  , PackageSet.PackageName(..)
  , PackagesFilter(..)
  ) where

import           Spago.Prelude

import qualified Control.Concurrent.Async.Pool as Async
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified System.Process                as Process

import           Spago.Config                  (Config (..))
import qualified Spago.Config                  as Config
import qualified Spago.Messages                as Messages
import           Spago.PackageSet              (Package (..), PackageName (..), PackageSet,
                                                Repo (..))
import qualified Spago.PackageSet              as PackageSet
import qualified Spago.Purs                    as Purs
import qualified Spago.Templates               as Templates


-- | The directory in which spago will put its tempfiles
spagoDir :: Text
spagoDir = ".spago/"


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder (if needed)
--   - create an example `test` folder (if needed)
initProject :: Spago m => Bool -> m ()
initProject force = do
  echo "Initializing a sample project or migrating an existing one.."

  -- packages.dhall and spago.dhall overwrite can be forced
  liftIO $ PackageSet.makePackageSetFile force
  Config.makeConfig force

  -- If these directories (or files) exist, we skip copying "sample sources"
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  whenDirNotExists "src" $ do
    copyIfNotExists "src/Main.purs" Templates.srcMain

  whenDirNotExists "test" $ do
    copyIfNotExists "test/Main.purs" Templates.testMain

  copyIfNotExists ".gitignore" Templates.gitignore

  echo "Set up a local Spago project."
  echo "Try running `spago build`"

  where
    whenDirNotExists dir action = do
      let dirPath = pathFromText dir
      dirExists <- testdir dirPath
      case dirExists of
        True -> echo $ Messages.foundExistingDirectory dir
        False -> do
          mktree dirPath
          action

    copyIfNotExists dest srcTemplate = do
      let destPath = pathFromText dest
      (testfile destPath) >>= \case
        True  -> echo $ Messages.foundExistingFile dest
        False -> writeTextFile destPath srcTemplate


-- | Returns the dir path for a given package
--   If the package is from a remote git repo, return the .spago folder in which we cloned
--   Otherwise return the local folder
getPackageDir :: (PackageName, Package) -> Text
getPackageDir (PackageName{..}, Package{ repo = Remote _, ..})
  = spagoDir <> packageName <> "/" <> version
getPackageDir (_, Package{ repo = Local path })
  = path


getGlobs :: [(PackageName, Package)] -> [Purs.SourcePath]
getGlobs = map (\pair -> Purs.SourcePath $ getPackageDir pair <> "/src/**/*.purs")


-- | If the repo points to a remote git, fetch it in the .spago folder.
--   If it's a local directory do nothing
fetchPackage :: (PackageName, Package) -> IO ()
fetchPackage (PackageName package, Package { repo = Local path }) =
  echo $ Messages.foundLocalPackage package path
fetchPackage pair@(PackageName{..}, Package{ repo = Remote repo, ..} ) = do
  exists <- testdir $ pathFromText packageDir
  if exists
    then do
      echo $ quotedName <> " already installed"
    else do
      echo $ "Installing " <> quotedName
      withDirectory (pathFromText packageDir) $ do
        (systemStrictWithErr processWithNewCwd empty) >>= \case
          (ExitSuccess, _, _) -> pure ()
          (_, _stdout, stderr) -> die $ Messages.failedToInstallDep quotedName stderr
  where
    packageDir = getPackageDir pair

    quotedName = Messages.surroundQuote packageName

    -- TODO:
    -- It is theoretically possible to put a commit hash in the ref to fetch,
    -- however, for how the Github server works, unless a commit is less than
    -- one hour old, you cannot `fetch` by commit hash
    -- If you want a commit by sha, you have to clone the whole repo history
    -- for a given ref (like master) and then look for the commit sha
    -- However, on Github you can actually get a snapshot for a specific sha,
    -- by calling archives/sha.tar.gz link.
    -- So basically we can check if we are dealing with a Github url and a commit hash,
    -- and if yes we fetch the tar.gz (instead of running this git commands), otherwise we fail

    cmd = Text.intercalate " && "
           [ "git init"
           , "git remote add origin " <> repo
           , "git fetch origin " <> version
           , "git -c advice.detachedHead=false checkout FETCH_HEAD"
           ]

    -- Here we set the package directory as the cwd of the new process.
    -- This is the "right" way to do it (instead of using e.g.
    -- System.Directory.withCurrentDirectory), as that's apparently
    -- not thread-safe
    processWithNewCwd = (Process.shell (Text.unpack cmd))
      { Process.cwd = Just $ Text.unpack packageDir }


fetchPackages :: Spago m => Maybe Int -> [(PackageName, Package)] -> m ()
fetchPackages maybeLimit allDeps = do

  PackageSet.checkPursIsUpToDate

  -- We try to fetch a dep only if their dir doesn't exist
  depsToFetch <- (flip filterM) allDeps $ \dep -> do
    exists <- testdir $ pathFromText $ getPackageDir dep
    pure $ not exists

  let nOfDeps = List.length depsToFetch
  when (nOfDeps > 0) $
    echoStr $ "Installing " <> show nOfDeps <> " dependencies."

  -- By default we make one thread per dep to fetch, but this can be limited
  liftIO $ Async.withTaskGroup (fromMaybe nOfDeps maybeLimit) $ \taskGroup -> do
    asyncs <- for depsToFetch (Async.async taskGroup . fetchPackage)
    handle (handler asyncs) (for_ asyncs Async.wait)
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

-- | Return all the transitive dependencies of the current project
getProjectDeps :: Spago m => Config -> m [(PackageName, Package)]
getProjectDeps Config{..} = getTransitiveDeps packages dependencies


-- | Return the transitive dependencies of a list of packages
--   Code basically from here:
--   https://github.com/purescript/psc-package/blob/648da70ae9b7ed48216ed03f930c1a6e8e902c0e/app/Main.hs#L227
getTransitiveDeps :: Spago m => PackageSet -> [PackageName] -> m [(PackageName, Package)]
getTransitiveDeps packageSet deps =
  Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen dep
      | dep `Set.member` seen =
          die $ "Cycle in package dependencies at package " <> packageName dep
      | otherwise =
        case Map.lookup dep packageSet of
          Nothing ->
            die $ pkgNotFoundMsg dep
          Just info@Package{..} -> do
            m <- fold <$> traverse (go (Set.insert dep seen)) dependencies
            pure (Map.insert dep info m)

    pkgNotFoundMsg pkg =
      "Package `" <> packageName pkg <> "` does not exist in package set" <> extraHelp
      where
        extraHelp = case suggestedPkg of
          Just pkg' | Map.member pkg' packageSet ->
            ", but `" <> packageName pkg' <> "` does, did you mean that instead?"
          Just pkg' ->
            ", and nor does `" <> packageName pkg' <> "`"
          Nothing ->
            ""

        suggestedPkg = do
          sansPrefix <- Text.stripPrefix "purescript-" (packageName pkg)
          Just (PackageName sansPrefix)


getReverseDeps  :: PackageSet -> PackageName -> IO [(PackageName, Package)]
getReverseDeps db dep =
    List.nub <$> foldMap go (Map.toList db)
  where
    go pair@(packageName, Package {..}) =
      case List.find (== dep) dependencies of
        Nothing -> return mempty
        Just _ -> do
          innerDeps <- getReverseDeps db packageName
          return $ pair : innerDeps


-- | Fetch all dependencies into `.spago/`
install :: Spago m => Maybe Int -> [PackageName] -> m ()
install maybeLimit newPackages = do
  -- Make sure .spago exists
  mktree $ pathFromText spagoDir

  config@Config{..} <- Config.ensureConfig

  -- Try fetching the dependencies with the new names too
  let newConfig :: Config
      newConfig = config { dependencies = dependencies <> newPackages }
  deps <- getProjectDeps newConfig

  -- If the above doesn't fail, write the new packages to the config
  -- Also skip the write if there are no new packages to be written
  case newPackages of
    []         -> pure ()
    additional -> Config.addDependencies config additional

  fetchPackages maybeLimit deps


data PackagesFilter = TransitiveDeps | DirectDeps


-- | A list of the packages that can be added to this project
listPackages :: Spago m => Maybe PackagesFilter -> m ()
listPackages packagesFilter = do
  Config{..} <- Config.ensureConfig
  packagesToList :: [(PackageName, Package)] <- case packagesFilter of
    Nothing             -> pure $ Map.toList packages
    Just TransitiveDeps -> getTransitiveDeps packages dependencies
    Just DirectDeps     -> pure $ Map.toList
      $ Map.restrictKeys packages (Set.fromList dependencies)

  case packagesToList of
    [] -> echo "There are no dependencies listed in your spago.dhall"
    _  -> traverse_ echo $ formatPackageNames packagesToList

  where
    -- | Format all the package names from the configuration
    formatPackageNames :: [(PackageName, Package)] -> [Text]
    formatPackageNames pkgs =
      let
        longestName = maximum $ fmap (Text.length . packageName . fst) pkgs
        longestVersion = maximum $ fmap (Text.length . version . snd) pkgs

        renderPkg (PackageName{..},Package{..})
          = leftPad longestName packageName <> " "
          <> leftPad longestVersion version <> "   "
          <> Text.pack (show repo)
      in map renderPkg pkgs

    leftPad :: Int -> Text -> Text
    leftPad n s
      | Text.length s < n  = s <> Text.replicate (n - Text.length s) " "
      | otherwise = s


-- | Get source globs of dependencies listed in `spago.dhall`
sources :: Spago m => m ()
sources = do
  config <- Config.ensureConfig
  deps <- getProjectDeps config
  _ <- traverse echo $ fmap Purs.unSourcePath $ getGlobs deps
  pure ()


verify :: Spago m => Maybe Int -> Maybe PackageName -> m ()
verify maybeLimit maybePackage = do
  Config{..} <- Config.ensureConfig
  case maybePackage of
    -- If no package is specified, verify all of them
    Nothing -> verifyPackages packages (Map.toList packages)
    -- In case we have a package, search in the package set for it
    Just packageName -> do
      case Map.lookup packageName packages of
        Nothing -> die $ "No packages found with the name " <> Text.pack (show packageName)
        -- When verifying a single package we check the reverse deps/referrers
        -- because we want to make sure the it doesn't break them
        -- (without having to check the whole set of course, that would work
        -- as well but would be much slower)
        Just package -> do
          reverseDeps <- liftIO $ getReverseDeps packages packageName
          let toVerify = [(packageName, package)] <> reverseDeps
          verifyPackages packages toVerify
  where
    verifyPackages :: Spago m => PackageSet -> [(PackageName, Package)] -> m ()
    verifyPackages packageSet packages = do
      echo $ Messages.verifying $ length packages
      traverse_ (verifyPackage packageSet) (fst <$> packages)

    verifyPackage :: Spago m => PackageSet -> PackageName -> m ()
    verifyPackage packageSet name = do
      deps <- getTransitiveDeps packageSet [name]
      let globs = getGlobs deps
          quotedName = Messages.surroundQuote $ packageName name
      fetchPackages maybeLimit deps
      echo $ "Verifying package " <> quotedName
      Purs.compile globs []
      echo $ "Successfully verified " <> quotedName
