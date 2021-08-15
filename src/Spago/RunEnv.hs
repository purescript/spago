module Spago.RunEnv where

import Spago.Prelude
import Spago.Env

import           System.Console.ANSI (hSupportsANSIWithoutEmulation)
import qualified System.Environment  as Env
import qualified RIO
import qualified System.Info
import qualified Turtle

import qualified Spago.Config as Config
import qualified Spago.GlobalCache as Cache
import qualified Spago.FetchPackage as FetchPackage
import qualified Spago.Dhall as Dhall
import qualified Spago.Messages as Messages
import qualified Spago.PackageSet as PackageSet
import qualified Spago.Packages as Packages
import qualified Spago.Purs as Purs

-- | Given the global CLI options, it creates the Env for the Spago context
--   and runs the app
withEnv :: GlobalOptions -> RIO Env a -> IO a
withEnv GlobalOptions{..} app = do
  let logHandle = stderr
  let verbose = not globalQuiet && (globalVerbose || globalVeryVerbose)

  -- https://github.com/purescript/spago/issues/579
  maybeTerm <- Env.lookupEnv "TERM"
  let termDumb = maybeTerm == Just "dumb" || maybeTerm == Just "win"
  -- Check if the terminal supports color. On Windows 10, terminal colors are enabled
  -- here as a side effect. Returns Nothing if output is redirected to a file.
  supportsAnsi <- hSupportsANSIWithoutEmulation logHandle <&> (== Just True)
  -- Also support NO_COLOR spec https://no-color.org/
  noColor <- Env.lookupEnv "NO_COLOR" <&> isJust
  let useColor = globalUseColor && not termDumb && not noColor && supportsAnsi

  logOptions' <- logOptionsHandle logHandle verbose
  let logOptions
        = setLogUseTime globalVeryVerbose
        $ setLogUseLoc globalVeryVerbose
        $ setLogUseColor useColor
        $ setLogVerboseFormat True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let logFunc' :: LogFunc
        logFunc' = if globalQuiet
          then mkLogFunc $ \_ _ _ _ -> pure ()
          else logFunc

    let configPath = fromMaybe Config.defaultPath globalConfigPath

    globalCache <- do
      path <- case globalCacheConfig of
        Just SkipCache -> pure FetchPackage.localCacheDir
        _ -> runRIO logFunc' $ do
          RIO.logDebug "Running `getGlobalCacheDir`"
          Cache.getGlobalCacheDir
      pure $ GlobalCache path globalCacheConfig

    let env = Env
          { envLogFunc = logFunc'
          , envJobs = Jobs $ fromMaybe 20 globalJobs
          , envConfigPath = ConfigPath configPath
          , envGlobalCache = globalCache
          }
    runRIO env app


withPackageSetEnv
  :: (HasLogFunc env, HasConfigPath env)
  => RIO PackageSetEnv a
  -> RIO env a
withPackageSetEnv app = do
  envPackageSet <- getPackageSet
  envLogFunc <- view (the @LogFunc)
  runRIO PackageSetEnv{..} app


withReplEnv
  :: (HasEnv env)
  => Config
  -> Target
  -> RIO ReplEnv a
  -> RIO env a
withReplEnv Config{..} target app = do
  Env{..} <- getEnv
  let envPackageSet = packageSet
      envTarget = target
  runRIO ReplEnv{..} app

withInstallEnv'
  :: (HasEnv env)
  => Maybe Config
  -> RIO InstallEnv a
  -> RIO env a
withInstallEnv' maybeConfig app = do
  Env{..} <- getEnv
  envConfig@Config{..} <- case maybeConfig of
    Just c -> pure c
    Nothing -> getConfig
  let envPackageSet = packageSet
  runRIO InstallEnv{..} app

withInstallEnv
  :: (HasEnv env)
  => RIO InstallEnv a
  -> RIO env a
withInstallEnv = withInstallEnv' Nothing

withVerifyEnv
  :: HasEnv env
  => UsePsa
  -> RIO VerifyEnv a
  -> RIO env a
withVerifyEnv usePsa app = do
  Env{..} <- getEnv
  envPursCmd <- getPurs usePsa
  envPackageSet <- getPackageSet
  envConfig <- hush <$> Config.ensureConfig 
  runRIO VerifyEnv{..} app

withPublishEnv
  :: HasEnv env
  => RIO PublishEnv a
  -> RIO env a
withPublishEnv app = do
  Env{..} <- getEnv
  envConfig@Config{..} <- getConfig
  let envPackageSet = packageSet
  envGitCmd <- getGit
  envBowerCmd <- BowerCmd <$>
    -- workaround windows issue: https://github.com/haskell/process/issues/140
    case System.Info.os of
      "mingw32" -> do
        let bowers = Turtle.inproc "where" ["bower.cmd"] empty
        Turtle.lineToText <$> Turtle.single (Turtle.limit 1 bowers)
      _ -> findExecutableOrDie "bower"
  runRIO PublishEnv{..} app

withBuildEnv'
  :: HasEnv env
  => Maybe Config
  -> UsePsa
  -> BuildOptions
  -> RIO BuildEnv a
  -> RIO env a
withBuildEnv' maybeConfig usePsa envBuildOptions@BuildOptions{ noInstall } app = do
  Env{..} <- getEnv
  envPursCmd <- getPurs usePsa
  envConfig@Config{..} <- maybe getConfig pure maybeConfig
  let envPackageSet = packageSet
  deps <- runRIO InstallEnv{..} $ do
    deps <- Packages.getProjectDeps
    when (noInstall == DoInstall) $ FetchPackage.fetchPackages deps
    pure deps
  envGraph <- runRIO PursEnv{..} (getMaybeGraph envBuildOptions envConfig deps)
  envGitCmd <- getGit
  logDebug "Running in `BuildEnv`"
  runRIO BuildEnv{..} app

withBuildEnv
  :: HasEnv env
  => UsePsa
  -> BuildOptions
  -> RIO BuildEnv a
  -> RIO env a
withBuildEnv = withBuildEnv' Nothing

withPursEnv
  :: HasEnv env
  => UsePsa
  -> RIO PursEnv a
  -> RIO env a
withPursEnv usePsa app = do
  Env{..} <- getEnv
  envPursCmd <- getPurs usePsa
  runRIO PursEnv{..} app

getEnv :: HasEnv env => RIO env Env
getEnv = do
  envLogFunc <- view (the @LogFunc)
  envJobs <- view (the @Jobs)
  envConfigPath <- view (the @ConfigPath)
  envGlobalCache <- view (the @GlobalCache)
  pure Env{..}

getConfig :: (HasLogFunc env, HasConfigPath env) => RIO env Config
getConfig = Config.ensureConfig >>= \case
  Right c -> pure c
  Left err -> die [ "Failed to read the config. Error was:", err ]

getPurs :: HasLogFunc env => UsePsa -> RIO env PursCmd
getPurs usePsa = do
  purs <- findExecutableOrDie "purs"
  psa <- case usePsa of
    NoPsa -> pure Nothing
    UsePsa -> findExecutable "psa"
  compilerVersion <- Purs.pursVersion >>= \case
    Left err -> die [ "Failed to fetch purs version. Error was:", display err ]
    Right version -> pure version
  return $ PursCmd {..}

getGit :: HasLogFunc env => RIO env GitCmd
getGit = GitCmd <$> findExecutableOrDie "git"

getPackageSet :: (HasLogFunc env, HasConfigPath env) => RIO env PackageSet
getPackageSet = do
  -- Try to read a "packages.dhall" directly
  try (liftIO (Dhall.inputExpr $ "./" <> PackageSet.packagesPath)) >>= \case
    Right (Dhall.RecordLit ks) -> Config.parsePackageSet (Dhall.extractRecordValues ks)
    (_ :: Either SomeException (Dhall.DhallExpr Void))  -> do
      -- Try to read a "spago.dhall" and find the packages from there
      Config.ensureConfig >>= \case
        Right Config{ packageSet } -> pure packageSet
        Left err -> die [ display Messages.couldNotVerifySet, "Error was:", display err ]

getMaybeGraph :: HasPursEnv env => BuildOptions -> Config -> [(PackageName, Package)] -> RIO env Graph
getMaybeGraph BuildOptions{ depsOnly, sourcePaths } Config{ configSourcePaths } deps = do
  logDebug "Running `getMaybeGraph`"
  let partitionedGlobs = Packages.getGlobs deps depsOnly configSourcePaths
      globs = Packages.getGlobsSourcePaths partitionedGlobs <> sourcePaths
  supportsGraph <- Purs.hasMinPursVersion "0.14.0"
  if not supportsGraph
  then pure Nothing
  else do
    maybeGraph <- Purs.graph globs
    case maybeGraph of
      Right graph -> pure $ Just graph
      Left err -> do
        logWarn $ displayShow err
        pure Nothing
