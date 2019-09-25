module Spago.Build
  ( build
  , test
  , run
  , repl
  , bundleApp
  , bundleModule
  , docs
  , search
  , Watch (..)
  , NoBuild (..)
  , NoInstall (..)
  , BuildOptions (..)
  , Packages.DepsOnly (..)
  , NoSearch (..)
  , OpenDocs (..)
  , Purs.ExtraArg (..)
  , Purs.ModuleName (..)
  , Purs.SourcePath (..)
  , Purs.TargetPath (..)
  , Purs.WithMain (..)
  ) where

import           Spago.Prelude

import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.List            as List
import qualified System.FilePath.Glob as Glob
import qualified System.IO.Temp       as Temp
import           System.Directory (getCurrentDirectory)
import qualified Turtle               as Turtle
import qualified Web.Browser          as Browser

import qualified Spago.Config         as Config
import qualified Spago.FetchPackage   as Fetch
import qualified Spago.GlobalCache    as GlobalCache
import qualified Spago.Packages       as Packages
import qualified Spago.Purs           as Purs
import qualified Spago.Templates      as Templates
import qualified Spago.Watch          as Watch

import           Spago.Types          as PackageSet


data Watch = Watch | BuildOnce

-- | Flag to go through with the build step
--   or skip it, in the case of 'bundleApp' and 'bundleModule'.
data NoBuild = NoBuild | DoBuild

-- | Flag to skip the automatic installation of libraries on build
data NoInstall = NoInstall | DoInstall

data BuildOptions = BuildOptions
  { cacheConfig :: Maybe GlobalCache.CacheFlag
  , shouldWatch :: Watch
  , shouldClear :: Watch.ClearScreen
  , sourcePaths :: [Purs.SourcePath]
  , noInstall   :: NoInstall
  , pursArgs    :: [Purs.ExtraArg]
  , depsOnly    :: Packages.DepsOnly
  }

prepareBundleDefaults
  :: Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> (Purs.ModuleName, Purs.TargetPath)
prepareBundleDefaults maybeModuleName maybeTargetPath = (moduleName, targetPath)
  where
    moduleName = fromMaybe (Purs.ModuleName "Main") maybeModuleName
    targetPath = fromMaybe (Purs.TargetPath "index.js") maybeTargetPath


-- | Build the project with purs, passing through additional args and
--   eventually running some other action after the build
build :: Spago m => BuildOptions -> Maybe (m ()) -> m ()
build BuildOptions{..} maybePostBuild = do
  echoDebug "Running `spago build`"
  config@Config.Config{ packageSet = PackageSet.PackageSet{..}, ..} <- Config.ensureConfig
  deps <- Packages.getProjectDeps config
  case noInstall of
    DoInstall -> Fetch.fetchPackages cacheConfig deps packagesMinPursVersion
    NoInstall -> pure ()
  let allGlobs = Packages.getGlobs deps depsOnly configSourcePaths <> sourcePaths
      genCoreFnOpts = [ Purs.ExtraArg "--codegen", Purs.ExtraArg "corefn" ]
      buildAction = do
        case alternateBackend of 
          Nothing ->
              Purs.compile allGlobs pursArgs
          Just backend -> do
              let normalizedArgs = concatMap (fmap Purs.ExtraArg . Text.words . Purs.unExtraArg) pursArgs
              when (genCoreFnOpts `List.isInfixOf` normalizedArgs) $ 
                die "No need to pass `--codegen corefn` explicitly when using the `backend` option. Remove the argument to solve the error."
              Purs.compile allGlobs $ pursArgs ++ genCoreFnOpts

              shell backend empty >>= \case
                ExitSuccess   -> pure ()
                ExitFailure n -> die $ "Backend " <> surroundQuote backend <> " exited with error:" <> repr n
        fromMaybe (pure ()) maybePostBuild


  absoluteGlobs <- traverse makeAbsolute $ Text.unpack . Purs.unSourcePath <$> allGlobs
  absoluteJSGlobs <- traverse makeAbsolute $ Text.unpack . Purs.unSourcePath
    <$> (Packages.getJsGlobs deps depsOnly configSourcePaths <> sourcePaths)

  case shouldWatch of
    BuildOnce -> buildAction
    Watch     -> Watch.watch
      (Set.fromAscList $ fmap Glob.compile $ absoluteGlobs <> absoluteJSGlobs)
      shouldClear buildAction

-- | Start a repl
repl
  :: Spago m
  => Maybe GlobalCache.CacheFlag
  -> [PackageSet.PackageName]
  -> [Purs.SourcePath]
  -> [Purs.ExtraArg]
  -> Packages.DepsOnly
  -> m ()
repl cacheFlag newPackages sourcePaths pursArgs depsOnly = do
  echoDebug "Running `spago repl`"

  try Config.ensureConfig >>= \case
    Right config@Config.Config{..} -> do
      deps <- Packages.getProjectDeps config
      let globs = Packages.getGlobs deps depsOnly configSourcePaths <> sourcePaths
      Purs.repl globs pursArgs
    Left (err :: SomeException) -> do
      echoDebug $ tshow err
      cacheDir <- GlobalCache.getGlobalCacheDir
      Temp.withTempDirectory cacheDir "spago-repl-tmp" $ \dir -> do
        Turtle.cd (Turtle.decodeString dir)

        Packages.initProject False

        config@Config.Config{ packageSet = PackageSet.PackageSet{..}, ..} <- Config.ensureConfig

        let updatedConfig = Config.Config name (dependencies <> newPackages) (Config.packageSet config) alternateBackend configSourcePaths publishConfig

        deps <- Packages.getProjectDeps updatedConfig
        let globs = Packages.getGlobs deps depsOnly $ Config.configSourcePaths updatedConfig

        Fetch.fetchPackages cacheFlag deps packagesMinPursVersion

        Purs.repl globs pursArgs

-- | Test the project: compile and run "Test.Main"
--   (or the provided module name) with node
test :: Spago m => Maybe Purs.ModuleName -> BuildOptions -> [Purs.ExtraArg] -> m ()
test = runWithNode (Purs.ModuleName "Test.Main") (Just "Tests succeeded.") "Tests failed: "

-- | Run the project: compile and run "Main"
--   (or the provided module name) with node
run :: Spago m => Maybe Purs.ModuleName -> BuildOptions -> [Purs.ExtraArg] -> m ()
run = runWithNode (Purs.ModuleName "Main") Nothing "Running failed, exit code: "

-- | Run the project with node: compile and run with the provided ModuleName
--   (or the default one if that's missing)
runWithNode
  :: Spago m
  => Purs.ModuleName
  -> Maybe Text
  -> Text
  -> Maybe Purs.ModuleName
  -> BuildOptions
  -> [Purs.ExtraArg]
  -> m ()
runWithNode defaultModuleName maybeSuccessMessage failureMessage maybeModuleName buildOpts nodeArgs = do
  echoDebug "Running NodeJS"
  build buildOpts (Just nodeAction)
  where
    moduleName = fromMaybe defaultModuleName maybeModuleName
    args = Text.intercalate " " $ map Purs.unExtraArg nodeArgs
    contents = "#!/usr/bin/env node\n\n" <> "require('../output/" <> Purs.unModuleName moduleName <> "').main()"
    cmd = "node .spago/run.js " <> args
    nodeAction = do
      echoDebug $ "Writing .spago/run.js"
      writeTextFile ".spago/run.js" contents
      chmod executable ".spago/run.js"
      shell cmd empty >>= \case
        ExitSuccess   -> fromMaybe (pure ()) (echo <$> maybeSuccessMessage)
        ExitFailure n -> die $ failureMessage <> repr n

  -- | Bundle the project to a js file
bundleApp
  :: Spago m
  => Purs.WithMain
  -> Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> NoBuild
  -> BuildOptions
  -> m ()
bundleApp withMain maybeModuleName maybeTargetPath noBuild buildOpts =
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      bundleAction = Purs.bundle withMain moduleName targetPath
  in case noBuild of
    DoBuild -> build buildOpts (Just bundleAction)
    NoBuild -> bundleAction

-- | Bundle into a CommonJS module
bundleModule
  :: Spago m
  => Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> NoBuild
  -> BuildOptions
  -> m ()
bundleModule maybeModuleName maybeTargetPath noBuild buildOpts = do
  echoDebug "Running `bundleModule`"
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      jsExport = Text.unpack $ "\nmodule.exports = PS[\""<> Purs.unModuleName moduleName <> "\"];"
      bundleAction = do
        echo "Bundling first..."
        Purs.bundle Purs.WithoutMain moduleName targetPath
        -- Here we append the CommonJS export line at the end of the bundle
        try (with
              (appendonly $ pathFromText $ Purs.unTargetPath targetPath)
              ((flip hPutStrLn) jsExport))
          >>= \case
            Right _ -> echo $ "Make module succeeded and output file to " <> Purs.unTargetPath targetPath
            Left (n :: SomeException) -> die $ "Make module failed: " <> repr n
  case noBuild of
    DoBuild -> build buildOpts (Just bundleAction)
    NoBuild -> bundleAction

-- | A flag to skip patching the docs using @purescript-docs-search@.
data NoSearch = NoSearch | AddSearch
  deriving (Eq)

-- | Flag to open generated HTML documentation in browser
data OpenDocs = NoOpenDocs | DoOpenDocs
  deriving (Eq)

-- | Generate docs for the `sourcePaths` and run `purescript-docs-search build-index` to patch them.
docs
  :: Spago m
  => Maybe Purs.DocsFormat
  -> [Purs.SourcePath]
  -> Packages.DepsOnly
  -> NoSearch
  -> OpenDocs
  -> m ()
docs format sourcePaths depsOnly noSearch open = do
  echoDebug "Running `spago docs`"
  config@Config.Config{..} <- Config.ensureConfig
  deps <- Packages.getProjectDeps config
  echo "Generating documentation for the project. This might take a while..."
  Purs.docs docsFormat $ Packages.getGlobs deps depsOnly configSourcePaths <> sourcePaths

  when isHTMLFormat $ do
    when (noSearch == AddSearch) $ do
      echo "Making the documentation searchable..."
      writeTextFile ".spago/purescript-docs-search" Templates.docsSearch
      writeTextFile ".spago/docs-search-app.js"     Templates.docsSearchApp
      let cmd = "node .spago/purescript-docs-search build-index"
      echoDebug $ "Running `" <> cmd <> "`"
      shell cmd empty >>= \case
        ExitSuccess   -> pure ()
        ExitFailure n -> echo $ "Failed while trying to make the documentation searchable: " <> repr n
        
    link <- linkToIndexHtml
    let linkText = "Link: " <> link
    echo linkText

    when (open == DoOpenDocs) $ do
      echo "Opening in browser..."
      () <$ openLink link

  where
    docsFormat = fromMaybe Purs.Html format
    isHTMLFormat = docsFormat == Purs.Html

    linkToIndexHtml = do
      currentDir <- liftIO $ Text.pack <$> getCurrentDirectory
      return ("file://" <> currentDir <> "/generated-docs/html/index.html")
    
    openLink link = liftIO $ Browser.openBrowser (Text.unpack link)

-- | Start a search REPL.
search :: Spago m => m ()
search = do
  config@Config.Config{..} <- Config.ensureConfig
  deps <- Packages.getProjectDeps config

  echo "Building module metadata..."

  Purs.compile (Packages.getGlobs deps Packages.AllSources configSourcePaths)
    [ Purs.ExtraArg "--codegen"
    , Purs.ExtraArg "docs"
    ]

  writeTextFile ".spago/purescript-docs-search" Templates.docsSearch
  let cmd = "node .spago/purescript-docs-search search"
  echoDebug $ "Running `" <> cmd <> "`"
  viewShell $ callCommand $ Text.unpack cmd
