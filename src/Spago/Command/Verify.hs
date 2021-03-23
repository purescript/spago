module Spago.Command.Verify (verify) where

import Spago.Prelude
import Spago.Env

import qualified Data.Map                 as Map

import qualified Spago.FetchPackage       as Fetch
import qualified Spago.Messages           as Messages
import qualified Spago.Purs               as Purs
import qualified Spago.Packages           as Packages


verify
  :: forall env
  .  HasVerifyEnv env
  => CheckModulesUnique -> Maybe PackageName
  -> RIO env ()
verify chkModsUniq maybePackage = do
  logDebug "Running `spago verify`"

  PackageSet{..} <- view (the @PackageSet)

  case maybePackage of
    -- If no package is specified, verify all of them
    Nothing -> do
      verifyPackages (Map.toList packagesDB)
    -- In case we have a package, search in the package set for it
    Just packageName@(PackageName actualPackageName) -> do
      case Map.lookup packageName packagesDB of
        Nothing -> die [ "No packages found with the name " <> displayShow actualPackageName ]
        -- When verifying a single package we check the reverse deps/referrers
        -- because we want to make sure the it doesn't break them
        -- (without having to check the whole set of course, that would work
        -- as well but would be much slower)
        Just package -> do
          reverseDeps <- Packages.getReverseDeps packageName
          let toVerify = [(packageName, package)] <> reverseDeps
          verifyPackages toVerify
  case chkModsUniq of
    DoCheckModulesUnique -> compileEverything
    NoCheckModulesUnique -> pure ()
  where
    verifyPackages :: [(PackageName, Package)] -> RIO env ()
    verifyPackages packages = do
      logInfo $ display $ Messages.verifying $ length packages
      traverse_ verifyPackage (fst <$> packages)

    verifyPackage :: PackageName -> RIO env ()
    verifyPackage name = do
      deps <- Packages.getTransitiveDeps [name]
      let globs = Packages.getGlobs deps Packages.DepsOnly []
          quotedName = surroundQuote $ packageName name
      Fetch.fetchPackages deps
      logInfo $ display $ "Verifying package " <> quotedName
      compile globs
      logInfo $ display $ "Successfully verified " <> quotedName

    compileEverything :: RIO env ()
    compileEverything = do
      PackageSet{ packagesDB } <- view (the @PackageSet)
      let deps = Map.toList packagesDB
          globs = Packages.getGlobs deps Packages.DepsOnly []
      Fetch.fetchPackages deps
      logInfo "Compiling everything (will fail if module names conflict)"
      compile globs
      logInfo "Successfully compiled everything"

    compile :: [SourcePath] -> RIO env ()
    compile globs = do
      config <- view (the @(Maybe Config))
      case config >>= alternateBackend of
        Nothing ->
          Purs.compile globs []
        Just backend -> do
          Purs.compile globs [ PursArg "--codegen", PursArg "corefn" ]
          let backendCmd = backend -- In future there will be some arguments here
          logDebug $ "Running command `" <> display backendCmd <> "`"
          shell backendCmd empty >>= \case
            ExitSuccess   -> pure ()
            ExitFailure n -> die [ "Backend " <> displayShow backend <> " exited with error:" <> repr n ]