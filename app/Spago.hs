module Spago
  ( initProject
  , install
  , sources
  , build
  , test
  , repl
  , bundle
  , makeModule
  , printVersion
  , listPackages
  , ModuleName(..)
  , TargetPath(..)
  , SourcePath(..)
  , WithMain(..)
  , PursArg(..)
  , PackageName(..)
  ) where

import qualified Control.Concurrent.Async.Pool as Async
import           Control.Exception             (SomeException, handle, try)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Foldable                 (for_, traverse_)
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Traversable              (for)
import           Data.Version                  (showVersion)
import qualified Paths_spago                   as Pcli
import           System.IO                     (hPutStrLn)
import qualified System.Process                as Process
import qualified Turtle                        as T hiding (die, echo)

import qualified PscPackage
import           Spago.Config                  (Config (..),addDependencies)
import qualified Spago.Config                  as Config
import           Spago.Spacchetti              (Package (..), PackageName (..), Repo (..))
import qualified Spago.Templates               as Templates
import           Spago.Turtle




-- | The directory in which spago will put its tempfiles
spagoDir :: Text
spagoDir = ".spago/"


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder
--   - create an example `test` folder
initProject :: Bool -> IO ()
initProject force = do
  -- packages.dhall and spago.dhall overwrite can be forced
  PscPackage.makePackagesDhall force "init"
  Config.makeConfig force
  T.mktree "src"
  T.mktree "test"
  -- But the other files in the template are just skipped if already there.
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  copyIfNotExists "src/Main.purs" Templates.srcMain
  copyIfNotExists "test/Main.purs" Templates.testMain
  copyIfNotExists ".gitignore" Templates.gitignore
  echo "Set up a local Spago project."
  echo "Try running `spago install`"
  where
    copyIfNotExists dest srcTemplate = do
      let destPath = T.fromText dest
      (T.testfile destPath) >>= \case
        True  -> echo ("Found existing " <> surroundQuote dest <> ", not overwriting it")
        False -> T.writeTextFile destPath srcTemplate


-- | Returns the dir path for a given package
--   If the package is from a remote git repo, return the .spago folder in which we cloned
--   Otherwise return the local folder
getPackageDir :: (PackageName, Package) -> Text
getPackageDir (PackageName{..}, Package{ repo = Remote _, ..})
  = spagoDir <> packageName <> "/" <> version
getPackageDir (_, Package{ repo = Local path })
  = path

getGlobs :: [(PackageName, Package)] -> [Text]
getGlobs = map (\pair -> getPackageDir pair <> "/src/**/*.purs")


-- | If the repo points to a remote git, fetch it in the .spago folder.
--   If it's a local directory do nothing
getDep :: (PackageName, Package) -> IO ()
getDep (PackageName package, Package { repo = Local path }) =
  echo $ "Skipping package "
      <> surroundQuote package
      <> ", using local path: "
      <> surroundQuote path
getDep pair@(PackageName{..}, Package{ repo = Remote repo, ..} ) = do
  exists <- T.testdir $ T.fromText packageDir
  if exists
    then do
      echo $ quotedName <> " already installed"
    else do
      echo $ "Installing " <> quotedName
      withDirectory (T.fromText packageDir) $ do
        (T.systemStrictWithErr processWithNewCwd T.empty) >>= \case
          (T.ExitSuccess, _, _) -> pure ()
          (_, _stdout, stderr) -> do
            echo ("\nFailed to install dependency " <> quotedName)
            echo "\nGit output:"
            echo stderr
            die "Aborting installation.."
  where
    packageDir = getPackageDir pair

    quotedName = surroundQuote packageName

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

getAllDependencies :: Config -> [(PackageName, Package)]
getAllDependencies Config { dependencies = deps, packages = pkgs } =
  Map.toList $ List.foldl' go Map.empty deps
  where
    go acc dep
      | Map.member dep acc = acc
      | otherwise =
          case Map.lookup dep pkgs of
            -- lazy error handling, user gets crash
            Nothing -> error $ "Package " <> show dep <> " was missing from the package set."
            Just x@(Package { dependencies = innerDeps }) -> do
              let newAcc = List.foldl' go acc innerDeps
              Map.insert dep x newAcc

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
        Config.ensureConfig >>= flip addDependencies additional

  config <- Config.ensureConfig

  let deps = getAllDependencies config
  echoStr $ "Installing " <> show (List.length deps) <> " dependencies."
  Async.withTaskGroup limit $ \taskGroup -> do
    asyncs <- for deps $ \dep -> Async.async taskGroup $ getDep dep
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

-- | A list of the packages that can be added to this project
listPackages :: IO ()
listPackages = do
  config <- Config.ensureConfig
  traverse_ echo $ formatPackageNames config

  where
    -- | Format all the package names from the configuration
    formatPackageNames :: Config -> [Text]
    formatPackageNames Config { packages = pkgs } =
      let
        pkgsList = Map.toList pkgs

        longestName = maximum $ fmap (Text.length . packageName . fst) pkgsList
        longestVersion = maximum $ fmap (Text.length . version . snd) pkgsList

        renderPkg (PackageName{..},Package{..})
          = leftPad longestName packageName <> " "
          <> leftPad longestVersion version <> "   "
          <> Text.pack (show repo)
      in map renderPkg pkgsList

    leftPad :: Int -> Text -> Text
    leftPad n s
      | Text.length s < n  = s <> Text.replicate (n - Text.length s) " "
      | otherwise = s


-- | Get source globs of dependencies listed in `spago.dhall`
sources :: IO ()
sources = do
  config <- Config.ensureConfig
  let
    deps = getAllDependencies config
    globs = getGlobs deps
  _ <- traverse echo globs
  pure ()


-- | Build the project with purs, passing through
--   the additional args in the list
build :: (Maybe Int) -> [SourcePath] -> [PursArg] -> IO ()
build maybeLimit sourcePaths passthroughArgs = do
  config <- Config.ensureConfig
  install maybeLimit mempty
  let
    deps  = getAllDependencies config
    globs = getGlobs deps <> ["src/**/*.purs", "test/**/*.purs"] <> map unSourcePath sourcePaths
    paths = Text.intercalate " " $ surroundQuote <$> globs
    args  = Text.intercalate " " $ map unPursArg passthroughArgs
    cmd = "purs compile " <> args <> " " <> paths
  T.shell cmd T.empty >>= \case
    T.ExitSuccess -> echo "Build succeeded."
    T.ExitFailure n -> die ("Failed to build: " <> T.repr n)


newtype ModuleName = ModuleName { unModuleName :: T.Text }
newtype TargetPath = TargetPath { unTargetPath :: T.Text }
newtype SourcePath = SourcePath { unSourcePath :: T.Text }
newtype PursArg = PursArg { unPursArg :: T.Text }

data WithMain = WithMain | WithoutMain

repl :: [SourcePath] -> [PursArg] -> IO ()
repl sourcePaths passthroughArgs = do
  config <- Config.ensureConfig
  let
    deps  = getAllDependencies config
    globs = getGlobs deps <> ["src/**/*.purs", "test/**/*.purs"] <> map unSourcePath sourcePaths
    args  = Text.unpack <$> ["repl"] <> globs <> map unPursArg passthroughArgs
  T.view $ liftIO $ Process.callProcess "purs" args

-- | Test the project: compile and run the Test.Main
--   (or the provided module name) with node
test :: Maybe ModuleName -> Maybe Int -> [SourcePath] -> [PursArg] -> IO ()
test maybeModuleName maybeLimit paths passthroughArgs = do
  build maybeLimit paths passthroughArgs
  T.shell cmd T.empty >>= \case
    T.ExitSuccess   -> echo "Tests succeeded."
    T.ExitFailure n -> die $ "Tests failed: " <> T.repr n
  where
    moduleName = fromMaybe (ModuleName "Test.Main") maybeModuleName
    cmd = "node -e \"require('./output/" <> unModuleName moduleName <> "').main()\""


prepareBundleDefaults :: Maybe ModuleName -> Maybe TargetPath -> (ModuleName, TargetPath)
prepareBundleDefaults maybeModuleName maybeTargetPath = (moduleName, targetPath)
  where
    moduleName = fromMaybe (ModuleName "Main") maybeModuleName
    targetPath = fromMaybe (TargetPath "index.js") maybeTargetPath


-- | Bundle the project to a js file
bundle :: WithMain -> Maybe ModuleName -> Maybe TargetPath -> IO ()
bundle withMain maybeModuleName maybeTargetPath = do
  let ((ModuleName moduleName), (TargetPath targetPath))
        = prepareBundleDefaults maybeModuleName maybeTargetPath

      main = case withMain of
        WithMain    -> " --main " <> moduleName
        WithoutMain -> ""

      cmd
        = "purs bundle \"output/*/*.js\""
        <> " -m " <> moduleName
        <> main
        <> " -o " <> targetPath

  T.shell cmd T.empty >>= \case
    T.ExitSuccess   -> echo $ "Bundle succeeded and output file to " <> targetPath
    T.ExitFailure n -> die $ "Bundle failed: " <> T.repr n


-- | Bundle into a CommonJS module
makeModule :: Maybe ModuleName -> Maybe TargetPath -> IO ()
makeModule maybeModuleName maybeTargetPath = do
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      jsExport = Text.unpack $ "\nmodule.exports = PS[\""<> unModuleName moduleName <> "\"];"
  echo "Bundling first..."
  bundle WithoutMain (Just moduleName) (Just targetPath)
  -- Here we append the CommonJS export line at the end of the bundle
  try (T.with
        (T.appendonly $ T.fromText $ unTargetPath targetPath)
        ((flip hPutStrLn) jsExport))
    >>= \case
      Right _ -> echo $ "Make module succeeded and output file to " <> unTargetPath targetPath
      Left (n :: SomeException) -> die $ "Make module failed: " <> T.repr n


-- | Print out Spago version
printVersion :: IO ()
printVersion =
  echoStr $ showVersion Pcli.version
