module Spago.Packages
  ( initProject
  , install
  , sources
  , verify
  , listPackages
  , getGlobs
  , getProjectDeps
  , PackageSet.upgradePackageSet
  , PackageSet.freeze
  , PackageSet.PackageName(..)
  , PackagesFilter(..)
  , JsonFlag(..)
  ) where

import           Spago.Prelude

import           Data.Aeson               as Aeson
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           Spago.Config       (Config (..))
import qualified Spago.Config       as Config
import qualified Spago.FetchPackage as Fetch
import           Spago.GlobalCache  (CacheFlag (..))
import qualified Spago.Messages     as Messages
import           Spago.PackageSet   (Package (..), PackageName (..), PackageSet (..), Repo(..))
import qualified Spago.PackageSet   as PackageSet
import qualified Spago.Purs         as Purs
import qualified Spago.Templates    as Templates


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

  -- Get the latest version of the package set if possible
  PackageSet.upgradePackageSet

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


getGlobs :: [(PackageName, Package)] -> [Purs.SourcePath]
getGlobs = map (\pair
                 -> Purs.SourcePath $ Text.pack $ Fetch.getLocalCacheDir pair
                 <> "/src/**/*.purs")


-- | Return all the transitive dependencies of the current project
getProjectDeps :: Spago m => Config -> m [(PackageName, Package)]
getProjectDeps Config{..} = getTransitiveDeps packageSet dependencies


-- | Return the transitive dependencies of a list of packages
--   Code basically from here:
--   https://github.com/purescript/psc-package/blob/648da70ae9b7ed48216ed03f930c1a6e8e902c0e/app/Main.hs#L227
getTransitiveDeps :: Spago m => PackageSet -> [PackageName] -> m [(PackageName, Package)]
getTransitiveDeps PackageSet{..} deps = do
  echoDebug "Getting transitive deps"
  let (packageMap, notFoundErrors, cycleErrors) = foldMap (go Set.empty Set.empty Set.empty) deps

  handleErrors (Map.toList packageMap) (Set.toList notFoundErrors) (Set.toList cycleErrors)

  where
    handleErrors packageMap notFoundErrors cycleErrors
      | not (null cycleErrors) = die $ "The following packages have circular dependencies:\n" <> (Text.intercalate "\n" . fmap pkgCycleMsg) cycleErrors
      | not (null notFoundErrors) = die $ "The following packages do not exist in your package set:\n" <> (Text.intercalate "\n" . fmap pkgNotFoundMsg) notFoundErrors
      | otherwise = pure packageMap

    pkgCycleMsg (CycleError pkg) = "  - " <> packageName pkg

    pkgNotFoundMsg (NotFoundError pkg) = "  - " <> packageName pkg <> extraHelp
      where
        extraHelp = case suggestedPkg of
          Just pkg' | Map.member pkg' packagesDB ->
            ", but `" <> packageName pkg' <> "` does, did you mean that instead?"
          Just pkg' ->
            ", and nor does `" <> packageName pkg' <> "`"
          Nothing ->
            ""
        suggestedPkg = do
          sansPrefix <- Text.stripPrefix "purescript-" (packageName pkg)
          Just (PackageName sansPrefix)

    go seen notFoundErrors cycleErrors dep
      | dep `Set.member` seen =
          (packagesDB, notFoundErrors, Set.insert (CycleError dep) cycleErrors)
      | otherwise = case Map.lookup dep packagesDB of
          Nothing ->
            (packagesDB , Set.insert (NotFoundError dep) notFoundErrors, cycleErrors)
          Just packageInfo@Package{..} -> do
            let (m, notFoundErrors', cycleErrors') = foldMap (go (Set.insert dep seen) notFoundErrors cycleErrors) dependencies
            (Map.insert dep packageInfo m, notFoundErrors', cycleErrors')

newtype NotFoundError a = NotFoundError a deriving (Eq, Ord)
newtype CycleError a = CycleError a deriving (Eq, Ord)


getReverseDeps  :: PackageSet -> PackageName -> IO [(PackageName, Package)]
getReverseDeps packageSet@PackageSet{..} dep = do
    List.nub <$> foldMap go (Map.toList packagesDB)
  where
    go pair@(packageName, Package{..}) = do
      case List.find (== dep) dependencies of
        Nothing -> return mempty
        Just _ -> do
          innerDeps <- getReverseDeps packageSet packageName
          return $ pair : innerDeps


-- | Fetch all dependencies into `.spago/`
install :: Spago m => Maybe Int -> Maybe CacheFlag -> [PackageName] -> m ()
install maybeLimit cacheFlag newPackages = do
  echoDebug "Running `spago install`"
  config@Config{ packageSet = PackageSet{..}, ..} <- Config.ensureConfig

  -- Try fetching the dependencies with the new names too
  let newConfig :: Config
      newConfig = config { dependencies = dependencies <> newPackages }
  deps <- getProjectDeps newConfig

  -- If the above doesn't fail, write the new packages to the config
  -- Also skip the write if there are no new packages to be written
  case newPackages of
    []         -> pure ()
    additional -> Config.addDependencies config additional

  Fetch.fetchPackages maybeLimit cacheFlag deps packagesMinPursVersion


data PackagesFilter = TransitiveDeps | DirectDeps

data JsonFlag = JsonOutputNo | JsonOutputYes

data JsonPackageOutput = JsonPackageOutput
  { json_packageName :: !Text
  , json_repo        :: !Text
  , json_version     :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON JsonPackageOutput where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
    { fieldLabelModifier = drop 5
    }

encodeJsonPackageOutput :: JsonPackageOutput -> Text
encodeJsonPackageOutput = LT.toStrict . LT.decodeUtf8 . Aeson.encode

-- | A list of the packages that can be added to this project
listPackages :: Spago m => Maybe PackagesFilter -> JsonFlag -> m ()
listPackages packagesFilter jsonFlag = do
  echoDebug "Running `listPackages`"
  Config{packageSet = packageSet@PackageSet{..}, ..} <- Config.ensureConfig
  packagesToList :: [(PackageName, Package)] <- case packagesFilter of
    Nothing             -> pure $ Map.toList $ packagesDB
    Just TransitiveDeps -> getTransitiveDeps packageSet dependencies
    Just DirectDeps     -> pure $ Map.toList
      $ Map.restrictKeys packagesDB (Set.fromList dependencies)

  case packagesToList of
    [] -> echo "There are no dependencies listed in your spago.dhall"
    _  -> traverse_ echo $ formatPackageNames packagesToList

  where
    formatPackageNames = case jsonFlag of
      JsonOutputYes -> formatPackageNamesJson
      JsonOutputNo -> formatPackageNamesText

    -- | Format all the packages from the config in JSON
    formatPackageNamesJson :: [(PackageName, Package)] -> [Text]
    formatPackageNamesJson pkgs =
      let
        asJson (PackageName{..}, Package{ location = PackageSet.Remote{..}, ..})
          = JsonPackageOutput
              { json_packageName = packageName
              , json_repo = unRepo repo
              , json_version = version
              }
        asJson (PackageName{..}, Package { location = PackageSet.Local{..}, ..})
          = JsonPackageOutput
              { json_packageName = packageName
              , json_repo = localPath
              , json_version = "local"
              }
      in map (encodeJsonPackageOutput . asJson) pkgs

    -- | Format all the package names from the configuration
    formatPackageNamesText :: [(PackageName, Package)] -> [Text]
    formatPackageNamesText pkgs =
      let
        showVersion PackageSet.Remote{..} = version
        showVersion _ = "local"

        showLocation PackageSet.Remote{ repo = Repo repo } = "Remote " <> surroundQuote repo
        showLocation PackageSet.Local{..} = "Local " <> surroundQuote localPath

        longestName = maximum $ fmap (Text.length . packageName . fst) pkgs
        longestVersion = maximum $ fmap (Text.length . showVersion . location . snd) pkgs

        renderPkg (PackageName{..}, Package{..})
          = leftPad longestName packageName <> " "
          <> leftPad longestVersion (showVersion location) <> "   "
          <> showLocation location
      in map renderPkg pkgs

    leftPad :: Int -> Text -> Text
    leftPad n s
      | Text.length s < n  = s <> Text.replicate (n - Text.length s) " "
      | otherwise = s


-- | Get source globs of dependencies listed in `spago.dhall`
sources :: Spago m => m ()
sources = do
  echoDebug "Running `spago sources`"
  config <- Config.ensureConfig
  deps <- getProjectDeps config
  _ <- traverse echo $ fmap Purs.unSourcePath $ getGlobs deps
  pure ()


verify :: Spago m => Maybe Int -> Maybe CacheFlag -> Maybe PackageName -> m ()
verify maybeLimit cacheFlag maybePackage = do
  echoDebug "Running `spago verify`"
  Config{ packageSet = packageSet@PackageSet{..}, ..} <- Config.ensureConfig
  case maybePackage of
    -- If no package is specified, verify all of them
    Nothing -> verifyPackages packageSet (Map.toList packagesDB)
    -- In case we have a package, search in the package set for it
    Just packageName -> do
      case Map.lookup packageName packagesDB of
        Nothing -> die $ "No packages found with the name " <> Text.pack (show packageName)
        -- When verifying a single package we check the reverse deps/referrers
        -- because we want to make sure the it doesn't break them
        -- (without having to check the whole set of course, that would work
        -- as well but would be much slower)
        Just package -> do
          reverseDeps <- liftIO $ getReverseDeps packageSet packageName
          let toVerify = [(packageName, package)] <> reverseDeps
          verifyPackages packageSet toVerify
  where
    verifyPackages :: Spago m => PackageSet -> [(PackageName, Package)] -> m ()
    verifyPackages packageSet packages = do
      echo $ Messages.verifying $ length packages
      traverse_ (verifyPackage packageSet) (fst <$> packages)

    verifyPackage :: Spago m => PackageSet -> PackageName -> m ()
    verifyPackage packageSet@PackageSet{..} name = do
      deps <- getTransitiveDeps packageSet [name]
      let globs = getGlobs deps
          quotedName = surroundQuote $ packageName name
      Fetch.fetchPackages maybeLimit cacheFlag deps packagesMinPursVersion
      echo $ "Verifying package " <> quotedName
      Purs.compile globs []
      echo $ "Successfully verified " <> quotedName
