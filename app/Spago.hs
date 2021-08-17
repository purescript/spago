module Spago (main) where

import           Spago.Prelude
import           Spago.Env

import           Data.Version        (showVersion)
import qualified Paths_spago         as Pcli
import           Main.Utf8           (withUtf8)
import           Spago.CLI           (Command(..))

import qualified System.Environment  as Env
import qualified Spago.Build
import qualified Spago.Messages      as Messages
import qualified Spago.Packages
import qualified Spago.PackageSet
import qualified Spago.CLI           as CLI
import qualified Spago.RunEnv        as Run
import qualified Spago.Version
import qualified Spago.Command.Ls as Ls
import qualified Spago.Command.Path as Path
import qualified Spago.Command.Verify as Verify
import qualified Spago.Command.Init as Init


main :: IO ()
main = withUtf8 $ do
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  (command, globalOptions@GlobalOptions{..}) 
    <- CLI.options "Spago - manage your PureScript projects" CLI.parser

  let handleDefault :: MonadIO m => ShowVersion -> m ()
      handleDefault = \case
        DoShowVersion -> CLI.echo spagoVersion *> exitSuccess
        NoShowVersion -> pure ()

  Run.withEnv globalOptions $
    case command of

      -- ### Commands that need no env
      Default shouldShowVersion
        -> handleDefault shouldShowVersion

      -- ### Commands that need only a basic global env
      Init force noComments tag
        -> void $ Init.initProject force noComments tag
      Freeze
        -> Spago.PackageSet.freeze Spago.PackageSet.packagesPath
      Version
        -> CLI.echo spagoVersion
      Path whichPath buildOptions
        -> Path.showPaths buildOptions whichPath
      Repl targetName replPackageNames paths pursArgs depsOnly
        -> Spago.Build.repl targetName replPackageNames paths pursArgs depsOnly
      BundleApp targetName modName tPath shouldBuild buildOptions
        -> Spago.Build.bundleApp targetName WithMain modName tPath shouldBuild buildOptions globalUsePsa
      BundleModule targetName modName tPath shouldBuild buildOptions
        -> Spago.Build.bundleModule targetName modName tPath shouldBuild buildOptions globalUsePsa
      Script modulePath tag dependencies scriptBuildOptions
        -> Spago.Build.script modulePath tag dependencies scriptBuildOptions

      -- ### Commands that need an Env and a PureScript executable
      PackageSetUpgrade tag -> Run.withPursEnv NoPsa
        $ Spago.PackageSet.updatePackageSetVersion tag

      -- ### Commmands that need only a Package Set
      ListPackages jsonFlag -> Run.withPackageSetEnv
        $ Ls.listPackageSet jsonFlag

      -- ### Commands that need an "install environment": global options and a Config
      Install targetName packageNames -> Run.withInstallEnv2 targetName
        $ Spago.Packages.install packageNames
      ListDeps targetName jsonFlag transitiveFlag -> Run.withInstallEnv2 targetName
        $ Ls.listPackages transitiveFlag jsonFlag
      Sources targetName -> Run.withInstallEnv2 targetName
        $ Spago.Packages.sources

      -- ### Commands that need a "publish env": install env + git and bower
      BumpVersion dryRun spec -> Run.withPublishEnv
        $ Spago.Version.bumpVersion dryRun spec

      -- ### Commands that need a "verification env": a Package Set + purs
      Verify package -> Run.withVerifyEnv globalUsePsa
        $ Verify.verify NoCheckModulesUnique (Just package)
      VerifySet checkUniqueModules -> Run.withVerifyEnv globalUsePsa
        $ Verify.verify checkUniqueModules Nothing

      -- ### Commands that need a build environment: a config, build options and access to purs
      Build targetName buildOptions -> Run.withBuildEnv2 targetName globalUsePsa buildOptions
        $ Spago.Build.build Nothing
      Search targetName -> Run.withBuildEnv2 targetName globalUsePsa defaultBuildOptions
        $ Spago.Build.search
      Docs targetName format sourcePaths depsOnly noSearch openDocs ->
        let
          opts = defaultBuildOptions { depsOnly = depsOnly, sourcePaths = sourcePaths }
        in Run.withBuildEnv2 targetName globalUsePsa opts
            $ Spago.Build.docs format noSearch openDocs
      Test targetName modName buildOptions nodeArgs -> Run.withBuildEnv2 targetName globalUsePsa buildOptions
        $ Spago.Build.test modName nodeArgs
      Run targetName modName buildOptions nodeArgs -> Run.withBuildEnv2 targetName globalUsePsa buildOptions
        $ Spago.Build.run modName nodeArgs

      -- ### Legacy commands, here for smoother migration path to new ones
      Bundle -> die [ display Messages.bundleCommandRenamed ]
      MakeModule -> die [ display Messages.makeModuleCommandRenamed ]
      ListPackagesOld -> die [ display Messages.listPackagesCommandRenamed ]


-- | Print out Spago version
spagoVersion :: String
spagoVersion = showVersion Pcli.version
