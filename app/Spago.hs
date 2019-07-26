module Spago (main) where

import           Spago.Prelude

import qualified Data.Text           as Text
import           Data.Version        (showVersion)
import qualified GHC.IO.Encoding
import qualified Options.Applicative as Opts
import qualified Paths_spago         as Pcli
import qualified System.Environment  as Env
import qualified Turtle              as CLI

import           Spago.Build         (BuildOptions (..), ExtraArg (..),
                                      ModuleName (..), SourcePath (..),
                                      TargetPath (..), Watch (..),
                                      WithMain (..), NoBuild (..),
                                      NoInstall (..), DepsOnly (..))
import qualified Spago.Build
import           Spago.DryRun        (DryRun (..))
import           Spago.GlobalCache   (CacheFlag (..))
import           Spago.Messages      as Messages
import           Spago.Packages      (PackageName (..), PackagesFilter (..), JsonFlag(..))
import qualified Spago.Packages
import qualified Spago.PscPackage    as PscPackage
import qualified Spago.Purs          as Purs
import           Spago.Version       (VersionBump(..))
import qualified Spago.Version       as Version
import           Spago.Watch         (ClearScreen (..))


-- | Commands that this program handles
data Command

  -- | ### Commands for working with Spago projects
  --
  -- | Initialize a new project
  = Init Bool

  -- | Install (download) dependencies defined in spago.dhall
  | Install (Maybe CacheFlag) [PackageName]

  -- | Get source globs of dependencies in spago.dhall
  | Sources

  -- | Start a REPL.
  | Repl (Maybe CacheFlag) [PackageName] [SourcePath] [ExtraArg] DepsOnly

  -- | Generate documentation for the project and its dependencies
  | Docs (Maybe Purs.DocsFormat) [SourcePath] DepsOnly

  -- | Build the project paths src/ and test/ plus the specified source paths
  | Build BuildOptions

  -- | List available packages
  | ListPackages (Maybe PackagesFilter) JsonFlag

  -- | Verify that a single package is consistent with the Package Set
  | Verify (Maybe CacheFlag) PackageName

    -- | Verify that the Package Set is correct
  | VerifySet (Maybe CacheFlag)

  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) BuildOptions [ExtraArg]

  -- | Bump and tag a new version in preparation for release.
  | BumpVersion DryRun VersionBump

  -- | Run the project with some module, default Main
  | Run (Maybe ModuleName) BuildOptions [ExtraArg]

  -- | Bundle the project into an executable
  --   Builds the project before bundling
  | BundleApp (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

  -- | Bundle a module into a CommonJS module
  --   Builds the project before bundling
  | BundleModule (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

  -- | Upgrade the package-set to the latest release
  | PackageSetUpgrade

  -- | Freeze the package-set so it will be cached
  | Freeze

  -- | ### Commands for working with Psc-Package
  --
  --   Do the boilerplate of the local project setup to override and add arbitrary packages
  --   See the package-sets docs about this here:
  --   https://github.com/purescript/package-sets
  | PscPackageLocalSetup Bool

  -- | Do the Ins-Dhall-ation of the local project setup, equivalent to:
  --   ```sh
  --   NAME='local'
  --   TARGET=.psc-package/$NAME/.set/packages.json
  --   mktree -p .psc-package/$NAME/.set
  --   dhall-to-json --pretty <<< './packages.dhall' > $TARGET
  --   echo wrote packages.json to $TARGET
  --   ```
  | PscPackageInsDhall

  -- | Deletes the .psc-package folder
  | PscPackageClean


  -- | Show version
  | Version


  -- | Bundle the project into an executable (replaced by BundleApp)
  | Bundle

  -- | Bundle a module into a CommonJS module (replaced by BundleModule)
  | MakeModule

parser :: CLI.Parser (Command, GlobalOptions)
parser = do
  opts <- globalOptions
  command <- projectCommands <|> packageSetCommands <|> publishCommands <|> pscPackageCommands <|> otherCommands <|> oldCommands
  pure (command, opts)
  where
    cacheFlag =
      let wrap = \case
            "skip" -> Just SkipCache
            "update" -> Just NewCache
            _ -> Nothing
      in CLI.optional $ CLI.opt wrap "global-cache" 'c' "Configure the global caching behaviour: skip it with `skip` or force update with `update`"
    packagesFilter =
      let wrap = \case
            "direct"     -> Just DirectDeps
            "transitive" -> Just TransitiveDeps
            _            -> Nothing
      in CLI.optional $ CLI.opt wrap "filter" 'f' "Filter packages: direct deps with `direct`, transitive ones with `transitive`"
    versionBump =
      let spec = \case
            "major" -> Just Version.Major
            "minor" -> Just Version.Minor
            "patch" -> Just Version.Patch
            v | Right v' <- Version.parseVersion v -> Just $ Exact v'
            _ -> Nothing
      in CLI.arg spec "bump" "How to bump the version. Acceptable values: 'major', 'minor', 'patch', or a version (e.g. 'v1.2.3')."

    force   = CLI.switch "force" 'f' "Overwrite any project found in the current directory"
    verbose = CLI.switch "verbose" 'v' "Enable additional debug logging, e.g. printing `purs` commands"

    -- Note: the first constructor is the default when the flag is not provided
    watch       = bool BuildOnce Watch <$> CLI.switch "watch" 'w' "Watch for changes in local files and automatically rebuild"
    noInstall   = bool DoInstall NoInstall <$> CLI.switch "no-install" 'n' "Don't run the automatic installation of packages"
    depsOnly    = bool AllSources DepsOnly <$> CLI.switch "deps-only" 'd' "Only use sources from dependencies, skipping the project sources."
    clearScreen = bool NoClear DoClear <$> CLI.switch "clear-screen" 'l' "Clear the screen on rebuild (watch mode only)"
    noBuild     = bool DoBuild NoBuild <$> CLI.switch "no-build" 's' "Skip build step"
    noFormat    = bool DoFormat NoFormat <$> CLI.switch "no-config-format" 'F' "Disable formatting the configuration file `spago.dhall`"
    jsonFlag    = bool JsonOutputNo JsonOutputYes <$> CLI.switch "json" 'j' "Produce JSON output"
    dryRun      = bool DryRun NoDryRun <$> CLI.switch "no-dry-run" 'f' "Actually perform side-effects (the default is to describe what would be done)"
    usePsa      = bool UsePsa NoPsa <$> CLI.switch "no-psa" 'P' "Don't build with `psa`, but use `purs`"

    mainModule  = CLI.optional $ CLI.opt (Just . ModuleName) "main" 'm' "Module to be used as the application's entry point"
    toTarget    = CLI.optional $ CLI.opt (Just . TargetPath) "to" 't' "The target file path"
    docsFormat  = CLI.optional $ CLI.opt Purs.parseDocsFormat "format" 'f' "Docs output format (markdown | html | etags | ctags)"
    jobsLimit   = CLI.optional (CLI.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    nodeArgs         = many $ CLI.opt (Just . ExtraArg) "node-args" 'a' "Argument to pass to node (run/test only)"
    replPackageNames = many $ CLI.opt (Just . PackageName) "dependency" 'd' "Package name to add to the REPL as dependency"
    sourcePaths      = many $ CLI.opt (Just . SourcePath) "path" 'p' "Source path to include"

    packageName     = CLI.arg (Just . PackageName) "package" "Specify a package name. You can list them with `list-packages`"
    packageNames    = many $ CLI.arg (Just . PackageName) "package" "Package name to add as dependency"
    passthroughArgs = many $ CLI.arg (Just . ExtraArg) " ..any `purs compile` option" "Options passed through to `purs compile`; use -- to separate"

    buildOptions  = BuildOptions <$> cacheFlag <*> watch <*> clearScreen <*> sourcePaths <*> noInstall <*> passthroughArgs <*> depsOnly

    -- Note: by default we limit concurrency to 20
    globalOptions = GlobalOptions <$> verbose <*> noFormat <*> usePsa <*> fmap (fromMaybe 20) jobsLimit

    projectCommands = CLI.subcommandGroup "Project commands:"
      [ initProject
      , build
      , repl
      , test
      , run
      , bundleApp
      , bundleModule
      , docs
      ]

    initProject =
      ( "init"
      , "Initialize a new sample project, or migrate a psc-package one"
      , Init <$> force
      )

    build =
      ( "build"
      , "Install the dependencies and compile the current package"
      , Build <$> buildOptions
      )

    repl =
      ( "repl"
      , "Start a REPL"
      , Repl <$> cacheFlag <*> replPackageNames <*> sourcePaths <*> passthroughArgs <*> depsOnly
      )

    test =
      ( "test"
      , "Test the project with some module, default Test.Main"
      , Test <$> mainModule <*> buildOptions <*> nodeArgs
      )

    run =
      ( "run"
      , "Runs the project with some module, default Main"
      , Run <$> mainModule <*> buildOptions <*> nodeArgs
      )

    bundleApp =
      ( "bundle-app"
      , "Bundle the project into an executable"
      , BundleApp <$> mainModule <*> toTarget <*> noBuild <*> buildOptions
      )

    bundleModule =
      ( "bundle-module"
      , "Bundle the project into a CommonJS module"
      , BundleModule <$> mainModule <*> toTarget <*> noBuild <*> buildOptions
      )

    docs =
      ( "docs"
      , "Generate docs for the project and its dependencies"
      , Docs <$> docsFormat <*> sourcePaths <*> depsOnly
      )


    packageSetCommands = CLI.subcommandGroup "Package set commands:"
      [ install
      , sources
      , listPackages
      , verify
      , verifySet
      , upgradeSet
      , freeze
      ]

    install =
      ( "install"
      , "Install (download) all dependencies listed in spago.dhall"
      , Install <$> cacheFlag <*> packageNames
      )

    sources =
      ( "sources"
      , "List all the source paths (globs) for the dependencies of the project"
      , pure Sources
      )

    listPackages =
      ( "list-packages"
      , "List packages available in your packages.dhall"
      , ListPackages <$> packagesFilter <*> jsonFlag
      )

    verify =
      ( "verify"
      , "Verify that a single package is consistent with the Package Set"
      , Verify <$> cacheFlag <*> packageName
      )

    verifySet =
      ( "verify-set"
      , "Verify that the whole Package Set builds correctly"
      , VerifySet <$> cacheFlag
      )

    upgradeSet =
      ( "upgrade-set"
      , "Upgrade the upstream in packages.dhall to the latest package-sets release"
      , pure PackageSetUpgrade
      )

    freeze =
      ( "freeze"
      , "Recompute the hashes for the package-set"
      , pure Freeze
      )


    publishCommands = CLI.subcommandGroup "Publish commands:"
      [ bumpVersion
      ]

    bumpVersion =
      ( "bump-version"
      , "Bump and tag a new version, and generate bower.json, in preparation for release."
      , BumpVersion <$> dryRun <*> versionBump
      )


    pscPackageCommands = CLI.subcommandGroup "Psc-Package compatibility commands:"
      [ pscPackageLocalSetup
      , pscPackageInsDhall
      , pscPackageClean
      ]

    pscPackageLocalSetup =
      ( "psc-package-local-setup"
      , "Setup a local package set by creating a new packages.dhall"
      , PscPackageLocalSetup <$> force
      )

    pscPackageInsDhall =
      ( "psc-package-insdhall"
      , "Insdhall the local package set from packages.dhall"
      , pure PscPackageInsDhall
      )

    pscPackageClean =
      ( "psc-package-clean"
      , "Clean cached packages by deleting the .psc-package folder"
      , pure PscPackageClean
      )


    otherCommands = CLI.subcommandGroup "Other commands:"
      [ version
      ]

    version =
      ( "version"
      , "Show spago version"
      , pure Version
      )


    oldCommands = Opts.subparser $ Opts.internal <> bundle <> makeModule

    bundle =
      Opts.command "bundle" $ Opts.info (Bundle <$ mainModule <* toTarget <* noBuild <* buildOptions) mempty

    makeModule =
      Opts.command "make-module" $ Opts.info (MakeModule <$ mainModule <* toTarget <* noBuild <* buildOptions) mempty


main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- | Print out Spago version
  let printVersion = CLI.echo $ CLI.unsafeTextToLine $ Text.pack $ showVersion Pcli.version

  (command, globalOptions) <- CLI.options "Spago - manage your PureScript projects" parser
  (flip runReaderT) globalOptions $
    case command of
      Init force                            -> Spago.Packages.initProject force
      Install cacheConfig packageNames      -> Spago.Packages.install cacheConfig packageNames
      ListPackages packagesFilter jsonFlag  -> Spago.Packages.listPackages packagesFilter jsonFlag
      Sources                               -> Spago.Packages.sources
      Verify cacheConfig package            -> Spago.Packages.verify cacheConfig (Just package)
      VerifySet cacheConfig                 -> Spago.Packages.verify cacheConfig Nothing
      PackageSetUpgrade                     -> Spago.Packages.upgradePackageSet
      Freeze                                -> Spago.Packages.freeze
      Build buildOptions                    -> Spago.Build.build buildOptions Nothing
      Test modName buildOptions nodeArgs    -> Spago.Build.test modName buildOptions nodeArgs
      BumpVersion dryRun spec               -> Version.bumpVersion dryRun spec
      Run modName buildOptions nodeArgs     -> Spago.Build.run modName buildOptions nodeArgs
      Repl cacheConfig replPackageNames paths pursArgs depsOnly
        -> Spago.Build.repl cacheConfig replPackageNames paths pursArgs depsOnly
      BundleApp modName tPath shouldBuild buildOptions
        -> Spago.Build.bundleApp WithMain modName tPath shouldBuild buildOptions
      BundleModule modName tPath shouldBuild buildOptions
        -> Spago.Build.bundleModule modName tPath shouldBuild buildOptions
      Docs format sourcePaths depsOnly      -> Spago.Build.docs format sourcePaths depsOnly
      Version                               -> printVersion
      PscPackageLocalSetup force            -> PscPackage.localSetup force
      PscPackageInsDhall                    -> PscPackage.insDhall
      PscPackageClean                       -> PscPackage.clean
      Bundle                                -> die Messages.bundleCommandRenamed
      MakeModule                            -> die Messages.makeModuleCommandRenamed
