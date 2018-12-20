module Spago
  ( initProject
  , install
  , sources
  , build
  , test
  , bundle
  , makeModule
  , printVersion
  , ModuleName(..)
  , TargetPath(..)
  , WithMain(..)
  ) where

import qualified Control.Concurrent.Async.Pool as Async
import           Control.Exception             (Exception, SomeException, handle, onException,
                                                throwIO, try)
import           Data.Foldable                 (for_)
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Traversable              (for)
import           Data.Version                  (showVersion)
import qualified Dhall.Format                  as Dhall.Format
import qualified Dhall.Pretty                  as Dhall.Pretty
import qualified Paths_spago                   as Pcli
import           System.IO                     (hPutStrLn)
import qualified System.Process                as Process
import qualified Turtle                        as T

import qualified PscPackage
import           Spago.Config
import           Spago.Spacchetti              (Package (..), PackageName (..))
import qualified Spago.Templates               as Templates


-- | Generic Error that we throw on program exit.
--   We have it so that errors are displayed nicely to the user
--   (the default Turtle.die is not nice)
newtype SpagoError = SpagoError { _unError :: Text }
instance Exception SpagoError
instance Show SpagoError where
  show (SpagoError err) = Text.unpack err


echo :: Text -> IO ()
echo = T.printf (T.s T.% "\n")

echoStr :: String -> IO ()
echoStr = echo . Text.pack

die :: Text -> IO ()
die reason = throwIO $ SpagoError reason

surroundQuote :: Text -> Text
surroundQuote y = "\"" <> y <> "\""

-- | Manage a directory tree as a resource, deleting it if we except during the @action@
--   NOTE: you should make sure the directory doesn't exist before calling this.
withDirectory :: T.FilePath -> IO a -> IO a
withDirectory dir action = (T.mktree dir >> action) `onException` (T.rmtree dir)

-- | The directory in which spago will put its tempfiles
spagoDir :: Text
spagoDir = ".spago/"

spagoDhallText :: Text
spagoDhallText = "spago.dhall"

spagoDhallPath :: T.FilePath
spagoDhallPath = T.fromText spagoDhallText


-- | Copies over `spago.dhall` to set up a Spago project
makeConfig :: Bool -> IO ()
makeConfig force = do
  -- Make sure .spago exists
  T.mktree $ T.fromText spagoDir

  T.unless force $ do
    hasSpagoDhall <- T.testfile spagoDhallPath
    T.when hasSpagoDhall $ die
       $ "Found " <> spagoDhallText <> ": there's already a project here. "
      <> "Run `spago init --force` if you're sure you want to overwrite it."
  T.touch spagoDhallPath
  T.writeTextFile spagoDhallPath Templates.spagoDhall

  Dhall.Format.format Dhall.Pretty.Unicode (Just $ Text.unpack spagoDhallText)


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder
--   - create an example `test` folder
initProject :: Bool -> IO ()
initProject force = do
  PscPackage.makePackagesDhall force "init"
  makeConfig force
  T.mktree "src"
  T.mktree "test"
  -- TODO fail if these files exist
  T.writeTextFile "src/Main.purs" Templates.srcMain
  T.writeTextFile "test/Main.purs" Templates.testMain
  T.writeTextFile ".gitignore" Templates.gitignore
  echo "Set up a local Spago project."
  echo "Try running `spago install`"


-- | Checks that the Spago config is there and readable
ensureConfig :: IO Config
ensureConfig = do
  exists <- T.testfile spagoDhallPath
  T.unless exists $ makeConfig False
  configText <- T.readTextFile spagoDhallPath
  try (parseConfig configText) >>= \case
    Right config -> pure config
    Left (err :: ConfigReadError) -> throwIO err


-- | Returns the dir path for a given package
getPackageDir :: (PackageName, Package) -> Text
getPackageDir (PackageName{..}, Package{..})
  = spagoDir <> packageName <> "/" <> version


getGlobs :: [(PackageName, Package)] -> [Text]
getGlobs = map (\pair -> getPackageDir pair <> "/src/**/*.purs")


getDep :: (PackageName, Package) -> IO ()
getDep pair@(PackageName{..}, Package{..} ) = do
  exists <- T.testdir $ T.fromText packageDir
  if exists
    then do
      echo $ quotedName <> " already installed"
    else do
      echo $ "Installing " <> quotedName
      withDirectory (T.fromText packageDir) $ do
        try (T.sh shell) >>= \case
          Right _ -> pure ()
          Left (_err :: T.ExitCode) -> do
            die ("Failed to install dependency " <> quotedName)
  where
    packageDir = getPackageDir pair

    quotedName = surroundQuote packageName

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

    -- Swallow stdout here in the shell, we don't want the whole git output
    shell = T.streamWithErr processWithNewCwd T.empty



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
install :: Maybe Int -> IO ()
install maybeLimit = do
  config <- ensureConfig
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


-- | Get source globs of dependencies listed in `spago.dhall`
sources :: IO ()
sources = do
  config <- ensureConfig
  let
    deps = getAllDependencies config
    globs = getGlobs deps
  _ <- traverse echo globs
  pure ()


-- | Build the project with purs
build :: Maybe [Text] -> IO ()
build targets = do
  config <- ensureConfig
  let
    deps  = getAllDependencies config
    globs = getGlobs deps <> (fromMaybe ["src/**/*.purs", "test/**/*.purs"] targets)
    paths = Text.intercalate " " $ surroundQuote <$> globs
    cmd = "purs compile " <> paths
  T.shell cmd T.empty >>= \case
    T.ExitSuccess -> echo "Build succeeded."
    T.ExitFailure n -> do
      die ("Failed to build: " <> T.repr n)


newtype ModuleName = ModuleName { unModuleName :: T.Text }
newtype TargetPath = TargetPath { unTargetPath :: T.Text }

data WithMain = WithMain | WithoutMain


-- | Test the project: compile and run the Test.Main
--   (or the provided module name) with node
test :: Maybe ModuleName -> IO ()
test maybeModuleName = do
  -- TODO: should this also include `src`? I don't see why not
  build $ Just ["test/**/*.purs"]
  T.shell cmd T.empty >>= \case
    T.ExitSuccess   -> echo "Tests succeeded."
    T.ExitFailure n -> die $ "Tests failed: " <> T.repr n
  where
    moduleName = fromMaybe (ModuleName "Test.Main") maybeModuleName
    cmd = "node -e 'require(\"./output/" <> unModuleName moduleName <> "\").main()'"


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
