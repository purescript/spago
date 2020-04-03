module Spago (main) where

import           Spago.Prelude

import           Data.Version        (showVersion)
import qualified Paths_spago         as Pcli
import           Main.Utf8           (withUtf8)
import           Spago.CLI           (Command(..))

import qualified System.Environment  as Env
import qualified Spago.Build
import qualified Spago.GitHub
import qualified Spago.Messages      as Messages
import qualified Spago.Packages
import qualified Spago.CLI           as CLI
import qualified Spago.Version


main :: IO ()
main = withUtf8 $ do
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  (command, globalOptions) <- CLI.options "Spago - manage your PureScript projects" CLI.parser

  CLI.runWithEnv globalOptions $
    case command of
      -- Commands that need only a basic global env
      Init force noComments                 -> Spago.Packages.initProject force noComments
      PackageSetUpgrade                     -> Spago.Packages.upgradePackageSet
      Freeze                                -> Spago.Packages.freeze Spago.Packages.packagesPath
      Login                                 -> Spago.GitHub.login
      Version                               -> printVersion
      -- install env
      Install cacheConfig packageNames      -> Spago.Packages.install cacheConfig packageNames
      -- packageset env
      ListPackages jsonFlag                 -> Spago.Packages.listPackages Spago.Packages.PackageSetPackages jsonFlag
      -- install env? or config env
      ListDeps jsonFlag CLI.IncludeTransitive   -> Spago.Packages.listPackages Spago.Packages.TransitiveDeps jsonFlag
      -- install env? or config env
      ListDeps jsonFlag CLI.NoIncludeTransitive -> Spago.Packages.listPackages Spago.Packages.DirectDeps jsonFlag
      -- config env
      Sources                               -> Spago.Packages.sources
      -- verify env
      Verify cacheConfig package            -> CLI.runWithBuildEnv $ Spago.Packages.verify cacheConfig Spago.Packages.NoCheckModulesUnique (Just package)
      -- verify env
      VerifySet cacheConfig chkModsUniq     -> CLI.runWithBuildEnv $ Spago.Packages.verify cacheConfig chkModsUniq Nothing
      -- build env
      Build buildOptions                    -> CLI.runWithBuildEnv $ Spago.Build.build buildOptions Nothing
      -- bundle env
      Test modName buildOptions nodeArgs    -> CLI.runWithBuildEnv $ Spago.Build.test modName buildOptions nodeArgs
      -- bundle env
      Run modName buildOptions nodeArgs     -> CLI.runWithBuildEnv $ Spago.Build.run modName buildOptions nodeArgs
      -- bundle env
      BundleApp modName tPath shouldBuild buildOptions
        -> CLI.runWithBuildEnv $ Spago.Build.bundleApp Spago.Build.WithMain modName tPath shouldBuild buildOptions
      -- bundle env
      BundleModule modName tPath shouldBuild buildOptions
        -> CLI.runWithBuildEnv $ Spago.Build.bundleModule modName tPath shouldBuild buildOptions
      -- publish env
      BumpVersion dryRun spec               -> Spago.Version.bumpVersion dryRun spec
      -- repl env
      Repl cacheConfig replPackageNames paths pursArgs depsOnly
        -> Spago.Build.repl cacheConfig replPackageNames paths pursArgs depsOnly
      -- docs env
      Docs format sourcePaths depsOnly noSearch openDocs
        -> Spago.Build.docs format sourcePaths depsOnly noSearch openDocs
      -- build env
      Search                                -> CLI.runWithBuildEnv $ Spago.Build.search
      -- build env? or maybe just build options, etc
      Path whichPath buildOptions           -> Spago.Build.showPaths buildOptions whichPath
      -- ### Legacy commands, here for smoother migration path to new ones
      Bundle                                -> die [ display Messages.bundleCommandRenamed ]
      MakeModule                            -> die [ display Messages.makeModuleCommandRenamed ]
      ListPackagesOld                       -> die [ display Messages.listPackagesCommandRenamed ]


{-
publish env: git, bower
packageset env: packages.dhall or spago.dhall
install env: use cache?, spago.dhall
verify env: packageset env, has purs
build env: install env, build options, purs
bundle env: build env, bundle options
repl env: build env?
docs env: ...
-}

-- | Print out Spago version
printVersion :: RIO env ()
printVersion = CLI.echo (showVersion Pcli.version)