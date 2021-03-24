module Spago.RunEnv where

import Spago.Prelude
import Spago.Env

import qualified System.Environment  as Env
import qualified Distribution.System as OS
import qualified RIO
import qualified Turtle

import qualified Spago.Config as Config
import qualified Spago.GlobalCache as Cache
import qualified Spago.FetchPackage as FetchPackage
import qualified Spago.Dhall as Dhall
import qualified Spago.Messages as Messages
import qualified Spago.PackageSet as PackageSet
import qualified Spago.Purs as Purs

-- | Given the global CLI options, it creates the Env for the Spago context
--   and runs the app
withEnv :: GlobalOptions -> RIO Env a -> IO a
withEnv GlobalOptions{..} app = do
  let verbose = not globalQuiet && (globalVerbose || globalVeryVerbose)

  -- https://github.com/purescript/spago/issues/579
  maybeTerm <- Env.lookupEnv "TERM"
  let termDumb = maybeTerm == Just "dumb" || maybeTerm == Just "win"
  let useColor = globalUseColor && not termDumb

  let logHandle = stderr
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


withInstallEnv'
  :: (HasEnv env)
  => Maybe Config
  -> UsePsa
  -> RIO InstallEnv a
  -> RIO env a
withInstallEnv' maybeConfig usePsa app = do
  Env{..} <- getEnv
  envConfig@Config{..} <- case maybeConfig of
    Just c -> pure c
    Nothing -> getConfig
  let envPackageSet = packageSet
  envPursCmd <- getPurs usePsa
  runRIO InstallEnv{..} app

withInstallEnv
  :: (HasEnv env)
  => UsePsa
  -> RIO InstallEnv a
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
    case OS.buildOS of
      OS.Windows -> do
        let bowers = Turtle.inproc "where" ["bower.cmd"] empty
        Turtle.lineToText <$> Turtle.single (Turtle.limit 1 bowers)
      _ -> findExecutableOrDie "bower"
  runRIO PublishEnv{..} app

withBuildEnv'
  :: HasEnv env
  => Maybe Config
  -> UsePsa
  -> RIO BuildEnv a
  -> RIO env a
withBuildEnv' maybeConfig usePsa app = do
  Env{..} <- getEnv
  envPursCmd <- getPurs usePsa
  envConfig@Config{..} <- case maybeConfig of
    Nothing -> getConfig
    Just c -> pure c
  let envPackageSet = packageSet
  envGitCmd <- getGit
  runRIO BuildEnv{..} app

withBuildEnv
  :: HasEnv env
  => UsePsa
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
  -- first we decide if we _want_ to use psa, then if we _can_
  pursCandidate <- case usePsa of
    NoPsa -> pure "purs"
    UsePsa -> findExecutable "psa" >>= \case
      Just _  -> pure "psa"
      Nothing -> pure "purs"
  -- We first try this for Windows
  purs <- case OS.buildOS of
    OS.Windows -> do
      findExecutable (pursCandidate <> ".cmd") >>= \case
        Just _ -> pure (Text.pack pursCandidate <> ".cmd")
        Nothing -> findExecutableOrDie pursCandidate
    _ -> findExecutableOrDie pursCandidate
  compilerVersion <- Purs.pursVersion purs >>= \case
    Left _ -> die [ "Failed to fetch purs version" ]
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
