module Spago.Packages
  ( initProject
  , install
  , sources
  , getGlobs
  , getJsGlobs
  , getDirectDeps
  , getProjectDeps
  , getReverseDeps
  , getTransitiveDeps
  , DepsOnly(..)
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Control.Monad.State.Lazy as State
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text

import qualified Spago.Config             as Config
import qualified Spago.Dhall              as Dhall
import qualified Spago.FetchPackage       as Fetch
import qualified Spago.Messages           as Messages
import qualified Spago.PackageSet         as PackageSet
import qualified Spago.Templates          as Templates


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder (if needed)
--   - create an example `test` folder (if needed)
initProject 
  :: (HasGlobalCache env, HasLogFunc env, HasConfigPath env)
  => Force -> Dhall.TemplateComments -> Maybe Text
  -> RIO env Config
initProject force comments tag = do
  logInfo "Initializing a sample project or migrating an existing one.."

  -- packages.dhall and spago.dhall overwrite can be forced
  PackageSet.makePackageSetFile force comments
  config <- Config.makeConfig force comments

  -- Use the specified version of the package set (if specified).
  -- Otherwise, get the latest version of the package set if possible
  PackageSet.updatePackageSetVersion tag

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
  pure config

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


getGlobs :: [(PackageName, Package)] -> DepsOnly -> [SourcePath] -> [SourcePath]
getGlobs deps depsOnly configSourcePaths
  = map (\pair
          -> SourcePath $ Text.pack $ Fetch.getLocalCacheDir pair
          <> "/src/**/*.purs") deps
  <> case depsOnly of
    DepsOnly   -> []
    AllSources -> configSourcePaths


getJsGlobs :: [(PackageName, Package)] -> DepsOnly -> [SourcePath] -> [SourcePath]
getJsGlobs deps depsOnly configSourcePaths
  = map (\pair
          -> SourcePath $ Text.pack $ Fetch.getLocalCacheDir pair
          <> "/src/**/*.js") deps
  <> case depsOnly of
    DepsOnly   -> []
    AllSources -> SourcePath . Text.replace ".purs" ".js" . unSourcePath
      <$> configSourcePaths


-- | Return the direct dependencies of the current project
getDirectDeps 
  :: (HasLogFunc env, HasConfig env)
  => RIO env [(PackageName, Package)]
getDirectDeps = do
  Config { packageSet = PackageSet{..}, dependencies } <- view configL 
  for dependencies $ \dep ->
    case Map.lookup dep packagesDB of
      Nothing ->
        die [ display $ pkgNotFoundMsg packagesDB (NotFoundError dep) ]
      Just pkg ->
        pure (dep, pkg)

getProjectDeps 
  :: (HasLogFunc env, HasConfig env)
  => RIO env [(PackageName, Package)]
getProjectDeps = do
  Config{ dependencies } <- view configL
  getTransitiveDeps dependencies

-- | Return the transitive dependencies of a list of packages
getTransitiveDeps
  :: (HasLogFunc env, HasPackageSet env)
  => [PackageName] -> RIO env [(PackageName, Package)]
getTransitiveDeps deps = do
  logDebug "Getting transitive deps"
  PackageSet{..} <- view packageSetL 

  let
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
  
  let (packageMap, notFoundErrors, cycleErrors) = State.evalState (fold <$> traverse (go mempty) deps) mempty

  handleErrors (Map.toList packageMap) (Set.toList notFoundErrors) (Set.toList cycleErrors)


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

getReverseDeps  :: HasPackageSet env => PackageName -> RIO env [(PackageName, Package)]
getReverseDeps dep = do
    PackageSet{ packagesDB } <- view packageSetL
    List.nub <$> foldMap go (Map.toList packagesDB)
  where
    go pair@(packageName, Package{..}) = do
      case dep `elem` dependencies of
        False -> return mempty
        True -> do
          innerDeps <- getReverseDeps packageName
          return $ pair : innerDeps


-- | Fetch all dependencies into `.spago/`
install 
  :: HasInstallEnv env
  => [PackageName] -> RIO env ()
install newPackages = do
  logDebug "Running `spago install`"
  config@Config{ packageSet = PackageSet{..}, ..} <- view configL

  existingNewPackages <- reportMissingPackages $ classifyPackages packagesDB newPackages

  -- Try fetching the dependencies with the new names too
  let newConfig :: Config
      newConfig = config { Config.dependencies = dependencies <> existingNewPackages }
  mapRIO (set configL newConfig) $ do
    deps <- getProjectDeps

    -- If the above doesn't fail, write the new packages to the config
    -- Also skip the write if there are no new packages to be written
    case existingNewPackages of
      []         -> pure ()
      additional -> Config.addDependencies config additional

    Fetch.fetchPackages deps

reportMissingPackages :: HasLogFunc env => PackagesLookupResult -> RIO env [PackageName]
reportMissingPackages (PackagesLookupResult found foundWithoutPrefix notFound) = do
  unless (null notFound) $
    die $
      [ "The following packages do not exist in your package set:" ]
      <> (fmap (\(NotFoundError (PackageName packageName)) -> display $ "  - " <> packageName) $ List.sort notFound)

  for_ foundWithoutPrefix $ \(FoundWithoutPrefix (PackageName sansPrefix)) ->
    logInfo $ display
      $ "The package 'purescript-" <> sansPrefix <> "' was resolved to the '" <> sansPrefix <> "' package"
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


-- | Get source globs of dependencies listed in `spago.dhall`
sources :: (HasLogFunc env, HasConfig env) => RIO env ()
sources = do
  logDebug "Running `spago sources`"
  config <- view configL
  deps <- getProjectDeps
  traverse_ output
    $ fmap unSourcePath
    $ getGlobs deps AllSources
    $ configSourcePaths config
