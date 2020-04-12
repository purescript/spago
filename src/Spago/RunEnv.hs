module Spago.RunEnv where

import Spago.Prelude
import Spago.Env

import qualified System.Environment  as Env
import qualified Distribution.System as OS
import qualified Turtle

import qualified Spago.Config as Config
import qualified Spago.GlobalCache as Cache
import qualified Spago.Dhall as Dhall
import qualified Spago.Messages as Messages
import qualified Spago.PackageSet as PackageSet

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

    globalCache <- runRIO logFunc' $ do
      logDebug "Running `getGlobalCacheDir`"
      Cache.getGlobalCacheDir

    let env = Env
          { envLogFunc = logFunc'
          , envJobs = fromMaybe 20 globalJobs
          , envConfigPath = configPath
          , envGlobalCache = globalCache
          , envCacheConfig = globalCacheConfig
          }
    runRIO env app


withPackageSetEnv
  :: HasEnv env
  => RIO PackageSetEnv a
  -> RIO env a
withPackageSetEnv app = do
  packageSetSet <- getPackageSet
  packageSetEnvGlobal <- view envL
  runRIO PackageSetEnv{..} app


withInstallEnv'
  :: HasEnv env
  => Maybe Config 
  -> RIO InstallEnv a
  -> RIO env a
withInstallEnv' maybeConfig app = do
  installSpagoConfig <- case maybeConfig of
    Just c -> pure c
    Nothing -> Config.ensureConfig >>= \case
      Right c -> pure c
      Left err -> die [ "Failed to read the config. Error was:", err ]
  installEnvGlobal <- view envL
  runRIO InstallEnv{..} app

withInstallEnv :: HasEnv env => RIO InstallEnv a -> RIO env a
withInstallEnv = withInstallEnv' Nothing

withVerifyEnv :: HasEnv env => UsePsa -> RIO VerifyEnv a -> RIO env a 
withVerifyEnv usePsa app = do
  verifyEnvPurs <- getPurs usePsa
  verifyPackageSet <- getPackageSet
  verifyEnvGlobal <- view envL 
  runRIO VerifyEnv{..} app

withPublishEnv :: HasEnv env => RIO PublishEnv a -> RIO env a
withPublishEnv app = withInstallEnv $ do
  publishEnvGit <- findExecutableOrDie "git"
  publishEnvBower <-
    -- workaround windows issue: https://github.com/haskell/process/issues/140
    case OS.buildOS of
      OS.Windows -> do
        let bowers = Turtle.inproc "where" ["bower.cmd"] empty
        Turtle.lineToText <$> Turtle.single (Turtle.limit 1 bowers)
      _ ->
        findExecutableOrDie "bower"
  publishEnvInstall <- view installEnvL
  runRIO PublishEnv{..} app

withBuildEnv
  :: HasEnv env
  => UsePsa
  -> RIO BuildEnv a 
  -> RIO env a
withBuildEnv usePsa app = withInstallEnv $ do
  buildEnvPurs <- getPurs usePsa
  buildInstallEnv <- view installEnvL
  runRIO BuildEnv{..} app


getPurs :: HasLogFunc env => UsePsa -> RIO env Text
getPurs usePsa = do
  -- first we decide if we _want_ to use psa, then if we _can_
  pursCandidate <- case usePsa of
    NoPsa -> pure "purs"
    UsePsa -> findExecutable "psa" >>= \case
      Just _  -> pure "psa"
      Nothing -> pure "purs"
  findExecutableOrDie pursCandidate

getPackageSet :: (HasLogFunc env, HasConfigPath env) => RIO env PackageSet
getPackageSet = do
  -- Try to read a "packages.dhall" directly
  try (liftIO (Dhall.inputExpr $ "./" <> PackageSet.packagesPath)) >>= \case
    Right (Dhall.RecordLit ks) -> Config.parsePackageSet ks
    (_ :: Either SomeException (Dhall.DhallExpr Void))  -> do
      -- Try to read a "spago.dhall" and find the packages from there
      Config.ensureConfig >>= \case
        Right Config{ packageSet } -> pure packageSet
        Left err -> die [ display Messages.couldNotVerifySet, "Error was:", display err ]