module Spago.Packages
  ( initProject
  , install
  , sources
  , verify
  , listPackages
  , getGlobs
  , getJsGlobs
  , getDirectDeps
  , getProjectDeps
  , PackageSet.upgradePackageSet
  , PackageSet.freeze
  , PackageSet.packagesPath
  , PackagesFilter(..)
  , CheckModulesUnique(..)
  , JsonFlag(..)
  , DepsOnly(..)
  ) where

import           Spago.Prelude

import qualified Control.Monad.State.Lazy as State
import           Data.Aeson               as Aeson
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           Spago.Config             (Config (..))
import qualified Spago.Config             as Config
import qualified Spago.Dhall              as Dhall
import qualified Spago.FetchPackage       as Fetch
import           Spago.GlobalCache        (CacheFlag (..))
import qualified Spago.Messages           as Messages
import qualified Spago.PackageSet         as PackageSet
import qualified Spago.Purs               as Purs
import qualified Spago.Templates          as Templates

import           Spago.Types              as PackageSet


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder (if needed)
--   - create an example `test` folder (if needed)
initProject :: Bool -> Dhall.TemplateComments -> Spago ()
initProject force comments = do
  logInfo "Initializing a sample project or migrating an existing one.."

  -- packages.dhall and spago.dhall overwrite can be forced
  PackageSet.makePackageSetFile force comments
  Config.makeConfig force comments

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

  copyIfNotExists ".purs-repl" Templates.pursRepl

  logInfo "Set up a local Spago project."
  logInfo "Try running `spago build`"

  where
    whenDirNotExists dir action = do
      let dirPath = pathFromText dir
      dirExists <- testdir dirPath
      case dirExists of
        True -> logInfo $ display $ Messages.foundExistingDirectory dir
        False -> do
          mktree dirPath
          action

    copyIfNotExists dest srcTemplate = do
      testfile dest >>= \case
        True  -> logInfo $ display $ Messages.foundExistingFile dest
        False -> writeTextFile dest srcTemplate


-- | Only build deps and ignore input paths
data DepsOnly = DepsOnly | AllSources

getGlobs :: [(PackageName, Package)] -> DepsOnly -> [Purs.SourcePath] -> [Purs.SourcePath]
getGlobs deps depsOnly configSourcePaths
  = map (\pair
          -> Purs.SourcePath $ Text.pack $ Fetch.getLocalCacheDir pair
          <> "/src/**/*.purs") deps
  <> case depsOnly of
    DepsOnly   -> []
    AllSources -> configSourcePaths


getJsGlobs :: [(PackageName, Package)] -> DepsOnly -> [Purs.SourcePath] -> [Purs.SourcePath]
getJsGlobs deps depsOnly configSourcePaths
  = map (\pair
          -> Purs.SourcePath $ Text.pack $ Fetch.getLocalCacheDir pair
          <> "/src/**/*.js") deps
  <> case depsOnly of
    DepsOnly   -> []
    AllSources -> Purs.SourcePath . Text.replace ".purs" ".js" . Purs.unSourcePath
      <$> configSourcePaths


-- | Return the direct dependencies of the current project
getDirectDeps :: Config -> Spago [(PackageName, Package)]
getDirectDeps Config{..} = do
  let PackageSet{..} = packageSet
  for dependencies $ \dep ->
    case Map.lookup dep packagesDB of
      Nothing ->
        die [ display $ pkgNotFoundMsg packagesDB (NotFoundError dep) ]
      Just pkg ->
        pure (dep, pkg)


-- | Return all the transitive dependencies of the current project
getProjectDeps :: Config -> Spago [(PackageName, Package)]
getProjectDeps Config{..} = getTransitiveDeps packageSet dependencies


-- | Return the transitive dependencies of a list of packages
getTransitiveDeps :: PackageSet -> [PackageName] -> Spago [(PackageName, Package)]
getTransitiveDeps PackageSet{..} deps = do
  logDebug "Getting transitive deps"
  let (packageMap, notFoundErrors, cycleErrors) = State.evalState (fold <$> traverse (go mempty) deps) mempty

  handleErrors (Map.toList packageMap) (Set.toList notFoundErrors) (Set.toList cycleErrors)
  where
    handleErrors packageMap notFoundErrors cycleErrors
      | not (null cycleErrors) = die $ [ "The following packages have circular dependencies:" ] <> fmap pkgCycleMsg cycleErrors
      | not (null notFoundErrors) = die $ [ "The following packages do not exist in your package set:" ] <> fmap (pkgNotFoundMsg packagesDB) notFoundErrors
      | otherwise = pure packageMap

    pkgCycleMsg (CycleError (PackageName packageName)) = "  - " <> display packageName

    go seen dep
      | dep `Set.member` seen =
          pure (mempty, mempty, Set.singleton $ CycleError dep)
      | otherwise = do
          cache <- State.get
          case Map.lookup dep cache of
            Just allDeps ->
              pure (allDeps, mempty, mempty)
            Nothing | Just packageInfo@Package{..} <- Map.lookup dep packagesDB -> do
              (childDeps, notFoundErrors, cycleErrors) <- fold <$> traverse (go (Set.insert dep seen)) dependencies
              let allDeps = Map.insert dep packageInfo childDeps
              when (null notFoundErrors && null cycleErrors) $ do
                State.modify $ Map.insert dep allDeps
              pure (allDeps, notFoundErrors, cycleErrors)
            Nothing ->
              pure (mempty, Set.singleton $ NotFoundError dep, mempty)


pkgNotFoundMsg :: Map PackageName Package -> NotFoundError PackageName -> Utf8Builder
pkgNotFoundMsg packagesDB (NotFoundError pkg@(PackageName packageName)) = display $ "  - " <> packageName <> extraHelp
  where
    extraHelp = case suggestedPkg of
      Just pkg'@(PackageName suggested) | Map.member pkg' packagesDB ->
        ", but `" <> suggested <> "` does, did you mean that instead?"
      Just (PackageName suggested) ->
        ", and nor does `" <> suggested <> "`"
      Nothing ->
        ""
    suggestedPkg = stripPurescriptPrefix pkg


newtype NotFoundError a = NotFoundError a deriving (Eq, Ord)
newtype CycleError a = CycleError a deriving (Eq, Ord)
newtype FoundWithoutPrefix = FoundWithoutPrefix PackageName

getReverseDeps  :: PackageSet -> PackageName -> IO [(PackageName, Package)]
getReverseDeps packageSet@PackageSet{..} dep = do
    List.nub <$> foldMap go (Map.toList packagesDB)
  where
    go pair@(packageName, Package{..}) = do
      case dep `elem` dependencies of
        False -> return mempty
        True -> do
          innerDeps <- getReverseDeps packageSet packageName
          return $ pair : innerDeps


-- | Fetch all dependencies into `.spago/`
install :: Maybe CacheFlag -> [PackageName] -> Spago ()
install cacheFlag newPackages = do
  logDebug "Running `spago install`"
  config@Config{ packageSet = PackageSet{..}, ..} <- Config.ensureConfig

  existingNewPackages <- reportMissingPackages $ classifyPackages packagesDB newPackages

  -- Try fetching the dependencies with the new names too
  let newConfig :: Config
      newConfig = config { dependencies = dependencies <> existingNewPackages }
  deps <- getProjectDeps newConfig

  -- If the above doesn't fail, write the new packages to the config
  -- Also skip the write if there are no new packages to be written
  case existingNewPackages of
    []         -> pure ()
    additional -> Config.addDependencies config additional

  Fetch.fetchPackages cacheFlag deps packagesMinPursVersion

reportMissingPackages :: PackagesLookupResult -> Spago [PackageName]
reportMissingPackages (PackagesLookupResult found foundWithoutPrefix notFound) = do
  unless (null notFound) $
    die $
      [ "The following packages do not exist in your package set:" ]
      <> (fmap (\(NotFoundError (PackageName packageName)) -> display $ "  - " <> packageName) $ List.sort notFound)

  for_ foundWithoutPrefix $ \(FoundWithoutPrefix sansPrefix) ->
    logWarn
    $ display
    $ "The package 'purescript-" <> packageName sansPrefix <> "' was not found in your package set, but '"
    <> packageName sansPrefix <> "' was. Using that instead."
  pure found


classifyPackages :: Map PackageName a -> [PackageName] -> PackagesLookupResult
classifyPackages packagesDB =
    foldr classifyPackage (PackagesLookupResult [] [] [])
  where
    classifyPackage :: PackageName -> PackagesLookupResult -> PackagesLookupResult
    classifyPackage pkg (PackagesLookupResult found foundWithoutPrefix notFound)
      | Map.member pkg packagesDB        = PackagesLookupResult (pkg : found) foundWithoutPrefix notFound
      | Just sansPrefix <- stripPurescriptPrefix pkg,
        Map.member sansPrefix packagesDB = PackagesLookupResult (sansPrefix : found) (FoundWithoutPrefix sansPrefix : foundWithoutPrefix) notFound
      | otherwise                        = PackagesLookupResult found foundWithoutPrefix (NotFoundError pkg : notFound)


data PackagesLookupResult = PackagesLookupResult
  { _found              :: [PackageName]
  , _foundWithoutPrefix :: [FoundWithoutPrefix]
  , _notFound           :: [NotFoundError PackageName]
  }


stripPurescriptPrefix :: PackageName -> Maybe PackageName
stripPurescriptPrefix (PackageName name) =
  PackageName <$> Text.stripPrefix "purescript-" name


data PackagesFilter = TransitiveDeps | DirectDeps

data JsonFlag = JsonOutputNo | JsonOutputYes

data JsonPackageOutput = JsonPackageOutput
  { json_packageName :: !Text
  , json_repo        :: !Value
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
listPackages :: Maybe PackagesFilter -> JsonFlag -> Spago ()
listPackages packagesFilter jsonFlag = do
  logDebug "Running `listPackages`"
  Config{packageSet = packageSet@PackageSet{..}, ..} <- Config.ensureConfig
  packagesToList :: [(PackageName, Package)] <- case packagesFilter of
    Nothing             -> pure $ Map.toList packagesDB
    Just TransitiveDeps -> getTransitiveDeps packageSet dependencies
    Just DirectDeps     -> pure $ Map.toList
      $ Map.restrictKeys packagesDB (Set.fromList dependencies)

  case packagesToList of
    [] -> logWarn "There are no dependencies listed in your spago.dhall"
    _  -> traverse_ output $ formatPackageNames packagesToList

  where
    formatPackageNames = case jsonFlag of
      JsonOutputYes -> formatPackageNamesJson
      JsonOutputNo  -> formatPackageNamesText

    -- | Format all the packages from the config in JSON
    formatPackageNamesJson :: [(PackageName, Package)] -> [Text]
    formatPackageNamesJson pkgs =
      let
        asJson (PackageName{..}, Package{ location = loc@PackageSet.Remote{..}, ..})
          = JsonPackageOutput
              { json_packageName = packageName
              , json_repo = toJSON loc
              , json_version = version
              }
        asJson (PackageName{..}, Package { location = loc@PackageSet.Local{..}, ..})
          = JsonPackageOutput
              { json_packageName = packageName
              , json_repo = toJSON loc
              , json_version = "local"
              }
      in map (encodeJsonPackageOutput . asJson) pkgs

    -- | Format all the package names from the configuration
    formatPackageNamesText :: [(PackageName, Package)] -> [Text]
    formatPackageNamesText pkgs =
      let
        showVersion PackageSet.Remote{..} = version
        showVersion _                     = "local"

        showLocation PackageSet.Remote{ repo = Repo repo } = "Remote " <> surroundQuote repo
        showLocation PackageSet.Local{..}                  = "Local " <> surroundQuote localPath

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
sources :: Spago ()
sources = do
  logDebug "Running `spago sources`"
  config <- Config.ensureConfig
  deps <- getProjectDeps config
  traverse_ output
    $ fmap Purs.unSourcePath
    $ getGlobs deps AllSources
    $ Config.configSourcePaths config


data CheckModulesUnique = DoCheckModulesUnique | NoCheckModulesUnique

verify :: Maybe CacheFlag -> CheckModulesUnique -> Maybe PackageName -> Spago ()
verify cacheFlag chkModsUniq maybePackage = do
  logDebug "Running `spago verify`"

  -- @TODO Swap the order of these to check for `spago.dhall` first, once
  -- `ensureConfig` no longer calls `die` internally. See:
  -- https://github.com/spacchetti/spago/pull/515#pullrequestreview-329632196
  packageSet@PackageSet{..} <- do
    -- Try to read a "packages.dhall" directly
    try (liftIO (Dhall.inputExpr $ "./" <> PackageSet.packagesPath)) >>= \case
      Right (Dhall.RecordLit ks) -> Config.parsePackageSet ks
      (_ :: Either SomeException (Dhall.DhallExpr Void))  -> do
          -- Try to read a "spago.dhall" and find the packages from there
          try Config.ensureConfig >>= \case
            Right (Config{ packageSet = packageSet@PackageSet{..}, ..}) -> pure packageSet
            Left (_ :: SomeException) -> die [ display Messages.couldNotVerifySet ]

  case maybePackage of
    -- If no package is specified, verify all of them
    Nothing -> do
      verifyPackages packageSet (Map.toList packagesDB)
    -- In case we have a package, search in the package set for it
    Just packageName@(PackageName actualPackageName) -> do
      case Map.lookup packageName packagesDB of
        Nothing -> die [ "No packages found with the name " <> displayShow actualPackageName ]
        -- When verifying a single package we check the reverse deps/referrers
        -- because we want to make sure the it doesn't break them
        -- (without having to check the whole set of course, that would work
        -- as well but would be much slower)
        Just package -> do
          reverseDeps <- liftIO $ getReverseDeps packageSet packageName
          let toVerify = [(packageName, package)] <> reverseDeps
          verifyPackages packageSet toVerify
  case chkModsUniq of
    DoCheckModulesUnique -> compileEverything packageSet
    NoCheckModulesUnique -> pure ()
  where
    verifyPackages :: PackageSet -> [(PackageName, Package)] -> Spago ()
    verifyPackages packageSet packages = do
      logInfo $ display $ Messages.verifying $ length packages
      traverse_ (verifyPackage packageSet) (fst <$> packages)

    verifyPackage :: PackageSet -> PackageName -> Spago ()
    verifyPackage packageSet@PackageSet{..} name = do
      deps <- getTransitiveDeps packageSet [name]
      let globs = getGlobs deps DepsOnly []
          quotedName = surroundQuote $ packageName name
      Fetch.fetchPackages cacheFlag deps packagesMinPursVersion
      logInfo $ display $ "Verifying package " <> quotedName
      Purs.compile globs []
      logInfo $ display $ "Successfully verified " <> quotedName

    compileEverything :: PackageSet -> Spago ()
    compileEverything PackageSet{..} = do
      let deps = Map.toList packagesDB
          globs = getGlobs deps DepsOnly []
      Fetch.fetchPackages cacheFlag deps packagesMinPursVersion
      logInfo "Compiling everything (will fail if module names conflict)"
      Purs.compile globs []
      logInfo "Successfully compiled everything"
