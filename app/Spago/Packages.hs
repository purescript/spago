module Spago.Packages
  ( initProject
  , install
  , sources
  , verify
  , listPackages
  , getGlobs
  , getProjectDeps
  , PackageSet.upgradeSpacchetti
  , PackageSet.PackageName(..)
  , PackagesFilter(..)
  ) where

import qualified Control.Concurrent.Async.Pool as Async
import           Control.Exception             (SomeException, handle)
import           Data.Foldable                 (fold, for_, traverse_)
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Traversable              (for)
import qualified System.Process                as Process
import qualified Turtle                        as T hiding (die, echo)

import           Spago.Config                  (Config (..))
import qualified Spago.Config                  as Config
import qualified Spago.Messages                as Messages
import           Spago.PackageSet              (Package (..), PackageName (..), PackageSet,
                                                Repo (..))
import qualified Spago.PackageSet              as PackageSet
import qualified Spago.Purs                    as Purs
import qualified Spago.Templates               as Templates
import           Spago.Turtle


-- | The directory in which spago will put its tempfiles
spagoDir :: Text
spagoDir = ".spago/"


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder (if needed)
--   - create an example `test` folder (if needed)
initProject :: Bool -> IO ()
initProject force = do
  -- packages.dhall and spago.dhall overwrite can be forced
  PackageSet.makePackageSetFile force
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
  echo "Try running `spago install`"

  where
    whenDirNotExists dir action = do
      let dirPath = T.fromText dir
      dirExists <- T.testdir dirPath
      case dirExists of
        True -> echo $ Messages.foundExistingDirectory dir
        False -> do
          T.mktree dirPath
          action

    copyIfNotExists dest srcTemplate = do
      let destPath = T.fromText dest
      (T.testfile destPath) >>= \case
        True  -> echo $ Messages.foundExistingFile dest
        False -> T.writeTextFile destPath srcTemplate


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
  exists <- T.testdir $ T.fromText packageDir
  if exists
    then do
      echo $ quotedName <> " already installed"
    else do
      echo $ "Installing " <> quotedName
      withDirectory (T.fromText packageDir) $ do
        (T.systemStrictWithErr processWithNewCwd T.empty) >>= \case
          (T.ExitSuccess, _, _) -> pure ()
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


fetchPackages :: Maybe Int -> [(PackageName, Package)] -> IO ()
fetchPackages maybeLimit deps = do

  PackageSet.checkPursIsUpToDate

  echoStr $ "Installing " <> show (List.length deps) <> " dependencies."
  Async.withTaskGroup limit $ \taskGroup -> do
    asyncs <- for deps $ \dep -> Async.async taskGroup $ fetchPackage dep
    handle (handler asyncs) $ for_ asyncs Async.wait
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
    handler asyncs (_e :: SomeException) = do
      for_ asyncs $ \async -> do
        Async.cancel async
        Async.waitCatch async
      die "Installation failed."

    -- We run a pretty high amount of threads by default, but this can be
    -- limited by specifying an option
    limit = fromMaybe 100 maybeLimit


-- | Return all the transitive dependencies of the current project
getProjectDeps :: Config -> IO [(PackageName, Package)]
getProjectDeps Config{..} = getTransitiveDeps packages dependencies


-- | Return the transitive dependencies of a list of packages
getTransitiveDeps :: PackageSet -> [PackageName] -> IO [(PackageName, Package)]
getTransitiveDeps packageSet deps =
  Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen dep
      | dep `Set.member` seen =
          die $ "Cycle in package dependencies at package " <> packageName dep
      | otherwise =
        case Map.lookup dep packageSet of
          Nothing ->
            die $ "Package " <> Text.pack (show dep) <> " was missing from the package set."
          Just info@Package{ .. } -> do
            m <- fold <$> traverse (go (Set.insert dep seen)) dependencies
            pure (Map.insert dep info m)


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
install :: Maybe Int -> [PackageName] -> IO ()
install maybeLimit packages = do
  -- Make sure .spago exists
  T.mktree $ T.fromText spagoDir

  -- Only call addDependencies if new packages are supplied
  case packages of
    [] -> pure ()
    additional ->
        -- Config is loaded here, because we will change it when
        -- adding packages, we will reload it later on
        -- (which will have the updates)
        Config.ensureConfig >>= flip Config.addDependencies additional

  config <- Config.ensureConfig

  deps <- getProjectDeps config
  fetchPackages maybeLimit deps


data PackagesFilter = TransitiveDeps | DirectDeps


-- | A list of the packages that can be added to this project
listPackages :: Maybe PackagesFilter -> IO ()
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
sources :: IO ()
sources = do
  config <- Config.ensureConfig
  deps <- getProjectDeps config
  _ <- traverse echo $ fmap Purs.unSourcePath $ getGlobs deps
  pure ()


verify :: Maybe Int -> Maybe PackageName -> IO ()
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
          reverseDeps <- getReverseDeps packages packageName
          let toVerify = [(packageName, package)] <> reverseDeps
          verifyPackages packages toVerify
  where
    verifyPackages :: PackageSet -> [(PackageName, Package)] -> IO ()
    verifyPackages packageSet packages = do
      echo $ Messages.verifying $ length packages
      traverse_ (verifyPackage packageSet) (fst <$> packages)

    verifyPackage :: PackageSet -> PackageName -> IO ()
    verifyPackage packageSet name = do
      deps <- getTransitiveDeps packageSet [name]
      let globs = getGlobs deps
          quotedName = Messages.surroundQuote $ packageName name
      fetchPackages maybeLimit deps
      echo $ "Verifying package " <> quotedName
      Purs.compile globs []
      echo $ "Successfully verified " <> quotedName
