module Spago where

import           Control.Exception (throwIO, try)
import qualified Data.List         as List
import qualified Data.Map          as Map
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Data.Version      (showVersion)
import qualified Dhall.Format      as Dhall.Format
import qualified Dhall.Pretty      as Dhall.Pretty
import qualified Paths_spago       as Pcli
import qualified System.Directory  as Dir
import qualified Turtle            as T

import qualified PscPackage
import           Spago.Config
import           Spago.Spacchetti  (Package (..), PackageName (..))
import qualified Spago.Templates   as Templates


echo' :: Text -> IO ()
echo' = T.echo . T.unsafeTextToLine

surroundQuote :: Text -> Text
surroundQuote y = "\"" <> y <> "\""

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
    T.when hasSpagoDhall $ T.die
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
init :: Bool -> IO ()
init force = do
  PscPackage.makePackagesDhall force "init"
  makeConfig force
  T.mktree "src"
  T.mktree "test"
  T.writeTextFile "src/Main.purs" Templates.srcMain
  T.writeTextFile "test/Main.purs" Templates.testMain
  T.writeTextFile ".gitignore" Templates.gitignore
  T.echo "Set up a local Spago project."
  T.echo "Try running `spago install`"


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
getDir :: (PackageName, Package) -> Text
getDir (PackageName{..}, Package{..})
  = spagoDir <> packageName <> "/" <> version


getGlobs :: [(PackageName, Package)] -> [Text]
getGlobs = map (\pair -> getDir pair <> "/src/**/*.purs")


getDep :: (PackageName, Package) -> IO ()
getDep pair@(PackageName{..}, Package{..} ) = do
  let dir = getDir pair
  exists <- T.testdir $ T.fromText dir
  if exists
    then do
      echo' $ surroundQuote packageName <> " already installed."
    else do
      echo' $ "Installing " <> surroundQuote packageName
      T.mktree . T.fromText $ dir
      Dir.withCurrentDirectory (Text.unpack dir) $ do
        let
          cmd = Text.intercalate " && "
            [ "git init"
            , "git remote add origin " <> repo
            , "git fetch origin " <> version
            , "git -c advice.detachedHead=false checkout FETCH_HEAD"
            ]
        -- Swallow stdout here, we don't want the whole git output.
        try (T.sh (T.inshellWithErr cmd T.empty)) >>= \case
          Right _ -> pure ()
          Left (err :: T.ExitCode) -> do
            T.rmtree $ T.fromText dir
            T.die ("Failed to install dependency: " <> T.repr err)


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
install :: IO ()
install = do
  config <- ensureConfig
  let deps = getAllDependencies config
  echo' $ "Installing " <> Text.pack (show $ List.length deps) <> " dependencies."
  _ <- traverse getDep deps
  T.echo "Installation complete."


-- | Get source globs of dependencies listed in `spago.dhall`
sources :: IO ()
sources = do
  config <- ensureConfig
  let
    deps = getAllDependencies config
    globs = getGlobs deps
  _ <- traverse (echo') globs
  pure ()


-- | Build the project with purs
build :: IO ()
build = do
  config <- ensureConfig
  let
    deps = getAllDependencies config
    globs = getGlobs deps <> ["src/**/*.purs", "test/**/*.purs"]
    paths = Text.intercalate " " $ surroundQuote <$> globs
    cmd = "purs compile " <> paths
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess -> T.echo "Build succeeded."
    T.ExitFailure n -> do
      T.die ("Failed to build:" <> T.repr n)


-- | Print out Spago version
printVersion :: IO ()
printVersion =
  echo' $ Text.pack $ showVersion Pcli.version
