module Spago.Build
  ( build
  , test
  , run
  , repl
  , bundleApp
  , bundleModule
  , docs
  , search
  , script
  ) where

import           Spago.Prelude hiding (link)
import           Spago.Env

import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import           System.Directory     (getCurrentDirectory)
import           System.FilePath      (splitDirectories)
import qualified System.FilePath.Glob as Glob
import qualified System.IO            as Sys
import qualified System.IO.Temp       as Temp
import qualified System.IO.Utf8       as Utf8
import           Text.Megaparsec      (parse)
import qualified Turtle
import qualified System.Process       as Process
import qualified Web.Browser          as Browser

import qualified Spago.Build.Parser   as Parse
import qualified Spago.Command.Path   as Path
import qualified Spago.RunEnv         as Run
import qualified Spago.Config         as Config
import qualified Spago.Dhall          as Dhall
import qualified Spago.FetchPackage   as Fetch
import qualified Spago.Messages       as Messages
import qualified Spago.Packages       as Packages
import qualified Spago.Purs           as Purs
import qualified Spago.Templates      as Templates
import qualified Spago.Watch          as Watch


prepareBundleDefaults
  :: Maybe ModuleName
  -> Maybe TargetPath
  -> (ModuleName, TargetPath)
prepareBundleDefaults maybeModuleName maybeTargetPath = (moduleName, targetPath)
  where
    moduleName = fromMaybe (ModuleName "Main") maybeModuleName
    targetPath = fromMaybe (TargetPath "index.js") maybeTargetPath

--   eventually running some other action after the build
build
  :: HasBuildEnv env
  => BuildOptions -> Maybe (RIO Env ())
  -> RIO env ()
build BuildOptions{..} maybePostBuild = do
  logDebug "Running `spago build`"
  Config{..} <- view (the @Config)
  deps <- Packages.getProjectDeps
  case noInstall of
    DoInstall -> Fetch.fetchPackages deps
    NoInstall -> pure ()
  let allPsGlobs = Packages.getGlobs   deps depsOnly configSourcePaths <> sourcePaths
      allJsGlobs = Packages.getJsGlobs deps depsOnly configSourcePaths <> sourcePaths

      buildBackend globs = do
        case alternateBackend of
          Nothing ->
              Purs.compile globs pursArgs
          Just backend -> do
              when (isJust $ Purs.findFlag 'g' "codegen" pursArgs) $
                die
                  [ "Can't pass `--codegen` option to build when using a backend"
                  , "Hint: No need to pass `--codegen corefn` explicitly when using the `backend` option."
                  , "Remove the argument to solve the error"
                  ]
              Purs.compile globs $ pursArgs ++ [ PursArg "--codegen", PursArg "corefn" ]

              logDebug $ display $ "Compiling with backend \"" <> backend <> "\""
              let backendCmd = backend -- In future there will be some arguments here
              logDebug $ "Running command `" <> display backendCmd <> "`"
              shell backendCmd empty >>= \case
                ExitSuccess   -> pure ()
                ExitFailure n -> die [ "Backend " <> displayShow backend <> " exited with error:" <> repr n ]

      buildAction globs = do
        env <- Run.getEnv
        let action = buildBackend globs >> (runRIO env $ fromMaybe (pure ()) maybePostBuild)
        runCommands "Before" beforeCommands
        action `onException` (runCommands "Else" elseCommands)
        runCommands "Then" thenCommands

  case shouldWatch of
    BuildOnce -> buildAction allPsGlobs
    Watch -> do
      (psMatches, psMismatches) <- partitionGlobs $ unwrap <$> allPsGlobs
      (jsMatches, jsMismatches) <- partitionGlobs $ unwrap <$> allJsGlobs

      case NonEmpty.nonEmpty (psMismatches <> jsMismatches) of
        Nothing -> pure ()
        Just mismatches -> logWarn $ display $ Messages.globsDoNotMatchWhenWatching $ NonEmpty.nub $ Text.pack <$> mismatches

      absolutePSGlobs <- traverse makeAbsolute psMatches
      absoluteJSGlobs <- traverse makeAbsolute jsMatches

      Watch.watch
        (Set.fromAscList $ fmap (Glob.compile . collapse) . removeDotSpago $ absolutePSGlobs <> absoluteJSGlobs)
        shouldClear
        allowIgnored
        (buildAction (wrap <$> psMatches))

  where
    runCommands :: HasLogFunc env => Text -> [Text] -> RIO env ()
    runCommands label = traverse_ runCommand
      where
      runCommand command = shell command empty >>= \case
        ExitSuccess   -> pure ()
        ExitFailure n -> die [ repr label <> " command failed. exit code: " <> repr n ]

    partitionGlobs :: [Sys.FilePath] -> RIO env ([Sys.FilePath], [Sys.FilePath])
    partitionGlobs = foldrM go ([],[])
      where
      go sourcePath (matches, mismatches) = do
        let parentDir = Watch.globToParent $ Glob.compile sourcePath
        paths <- liftIO $ Glob.glob parentDir
        pure $ if null paths
          then (matches, parentDir : mismatches)
          else (sourcePath : matches, mismatches)

    wrap   = SourcePath . Text.pack
    unwrap = Text.unpack . unSourcePath
    removeDotSpago = filter (\glob -> ".spago" `notElem` (splitDirectories glob))
    collapse = Turtle.encodeString . Turtle.collapse . Turtle.decodeString

-- | Start a repl
repl
  :: (HasEnv env)
  => [PackageName]
  -> [SourcePath]
  -> [PursArg]
  -> Packages.DepsOnly
  -> RIO env ()
repl newPackages sourcePaths pursArgs depsOnly = do
  logDebug "Running `spago repl`"
  purs <- Run.getPurs NoPsa
  Config.ensureConfig >>= \case
    Right config -> Run.withInstallEnv' (Just config) (replAction purs)
    Left err -> do
      logDebug err
      GlobalCache cacheDir _ <- view (the @GlobalCache)
      Temp.withTempDirectory cacheDir "spago-repl-tmp" $ \dir -> do
        Turtle.cd (Turtle.decodeString dir)

        config@Config{ packageSet = PackageSet{..}, ..}
          <- Packages.initProject NoForce Dhall.WithComments Nothing

        let newConfig :: Config
            newConfig = config { Config.dependencies = dependencies <> newPackages }
        Run.withInstallEnv' (Just newConfig) (replAction purs)
  where
    replAction purs = do
      Config{..} <- view (the @Config)
      deps <- Packages.getProjectDeps
      -- we check that psci-support is in the deps, see #550
      unless (Set.member (PackageName "psci-support") (Set.fromList (map fst deps))) $ do
        die
          [ "The package called 'psci-support' needs to be installed for the repl to work properly."
          , "Run `spago install psci-support` to add it to your dependencies."
          ]
      let globs = Packages.getGlobs deps depsOnly (configSourcePaths <> sourcePaths)
      Fetch.fetchPackages deps
      runRIO purs $ Purs.repl globs pursArgs


-- | Test the project: compile and run "Test.Main"
--   (or the provided module name) with node
test
  :: HasBuildEnv env
  => Maybe ModuleName -> BuildOptions -> [BackendArg]
  -> RIO env ()
test maybeModuleName buildOpts extraArgs = do
  let moduleName = fromMaybe (ModuleName "Test.Main") maybeModuleName
  Config.Config { alternateBackend, configSourcePaths } <- view (the @Config)
  liftIO (foldMapM (Glob.glob . Text.unpack . unSourcePath) configSourcePaths) >>= \paths -> do
    results <- forM paths $ \path -> do
      content <- readFileBinary path
      pure $ Parse.checkModuleNameMatches (encodeUtf8 $ unModuleName moduleName) content
    if or results
      then do
        runBackend alternateBackend Nothing moduleName (Just "Tests succeeded.") "Tests failed: " buildOpts extraArgs
      else do
        die [ "Module '" <> (display . unModuleName) moduleName <> "' not found! Are you including it in your build?" ]


-- | Run the project: compile and run "Main"
--   (or the provided module name) with node
run
  :: HasBuildEnv env
  => Maybe ModuleName -> BuildOptions -> [BackendArg]
  -> RIO env ()
run maybeModuleName buildOpts extraArgs = do
  Config.Config { alternateBackend } <- view (the @Config)
  let moduleName = fromMaybe (ModuleName "Main") maybeModuleName
  runBackend alternateBackend Nothing moduleName Nothing "Running failed; " buildOpts extraArgs


-- | Run the select module as a script: init, compile, and run the provided module
script
  :: (HasEnv env)
  => Text
  -> Maybe Text
  -> [PackageName]
  -> RIO env ()
script modulePath tag packageDeps = do
  logDebug "Running `spago script`"
  moduleContents <- readTextFile (pathFromText modulePath)
  moduleName <- case parse Parse.moduleDecl "" (encodeUtf8 moduleContents) of
    Left _ -> die [ "Can't extract module name from input module path" ]
    Right (Parse.PsModule name _) -> case decodeUtf8' name of
      Left _ -> die [ "Can't extract module name from input module path" ]
      Right modName -> pure (ModuleName modName)
  GlobalCache cacheDir _ <- view (the @GlobalCache)
  currentDir <- Turtle.pwd
  Temp.withTempDirectory cacheDir "spago-script-tmp" $ \dir -> do
    let tmpDir = Text.pack dir
    Turtle.cd (Turtle.fromText tmpDir)

    -- TODO: Add flag to opt in to creating src/ & test/ directory
    -- Do we really need all default dependencies? psci-support?
    -- Probably a datatype for flag instead of boolean
    -- script doesn't need psci-support or created files
    config@Config { packageSet = PackageSet{..}, ..}
      <- Packages.initProject NoForce Dhall.WithComments tag

    let newConfig :: Config
        newConfig = config
          { Config.dependencies = dependencies <> packageDeps
          , Config.configSourcePaths = []
          }

    Run.withInstallEnv' (Just newConfig) installAction

    Run.withBuildEnv' (Just newConfig) NoPsa (runAction moduleName (tmpDir, currentDir))
  where
    installAction = do
      deps <- Packages.getProjectDeps
      Fetch.fetchPackages deps

    runAction moduleName dirs = do
      let
        buildOpts = BuildOptions
          { shouldWatch = BuildOnce
          , shouldClear = NoClear
          , allowIgnored = DoAllowIgnored
          , sourcePaths = [ SourcePath modulePath ]
          , withSourceMap = WithoutSrcMap
          , noInstall = NoInstall
          , pursArgs = [] -- should we use this?
          , depsOnly = AllSources -- should this be DepsOnly?
          , beforeCommands = []
          , thenCommands = []
          , elseCommands = []
          }
      runBackend Nothing (Just dirs) moduleName Nothing "error" buildOpts []

-- | Run the project with node (or the chosen alternate backend):
--   compile and run the provided ModuleName
runBackend
  :: HasBuildEnv env
  => Maybe Text
  -> Maybe (Text, FilePath)
  -> ModuleName
  -> Maybe Text
  -> Text
  -> BuildOptions
  -> [BackendArg]
  -> RIO env ()
runBackend maybeBackend maybeTmp moduleName maybeSuccessMessage failureMessage buildOpts@BuildOptions{pursArgs} extraArgs = do
  logDebug $ display $ "Running with backend: " <> fromMaybe "nodejs" maybeBackend
  let postBuild = maybe (nodeAction $ Path.getOutputPath pursArgs) backendAction maybeBackend
  build buildOpts (Just postBuild)
  where
    sourceDirectory = case maybeTmp of
      Nothing -> ""
      Just (tmp, _) -> tmp <> "/"
    runJsSource = sourceDirectory <> ".spago/run.js"
    nodeArgs = Text.intercalate " " $ map unBackendArg extraArgs
    nodeContents outputPath' = case maybeTmp of
      Nothing ->
        "#!/usr/bin/env node\n\n" <> "require('../" <> Text.pack outputPath' <> "/" <> unModuleName moduleName <> "').main()"
      Just (tmp, _) ->
        "#!/usr/bin/env node\n\n" <> "require('" <> tmp <> "/" <> Text.pack outputPath' <> "/" <> unModuleName moduleName <> "').main()"
    nodeCmd = "node " <> runJsSource <> nodeArgs
    nodeAction outputPath' = do
      logDebug $ "Writing " <> displayShow @Text runJsSource
      writeTextFile runJsSource (nodeContents outputPath')
      void $ chmod executable $ pathFromText runJsSource
      -- If the project source is in a temp directory, then the process should
      -- be run from the directory which contains the temp directory.
      for_ maybeTmp $ \(_, current) -> do
        logDebug $ "Executing from: " <> displayShow @FilePath current
        Turtle.cd current
      -- We build a process by hand here because we need to forward the stdin to the backend process
      let processWithStdin = (Process.shell (Text.unpack nodeCmd)) { Process.std_in = Process.Inherit }
      Turtle.system processWithStdin empty >>= \case
        ExitSuccess   -> maybe (pure ()) (logInfo . display) maybeSuccessMessage
        ExitFailure n -> die [ display failureMessage <> "exit code: " <> repr n ]
    backendAction backend = do
      let args :: [Text] = ["--run", unModuleName moduleName <> ".main"] <> fmap unBackendArg extraArgs
      logDebug $ display $ "Running command `" <> backend <> " " <> Text.unwords args <> "`"
      Turtle.proc backend args empty >>= \case
        ExitSuccess   -> maybe (pure ()) (logInfo . display) maybeSuccessMessage
        ExitFailure n -> die [ display failureMessage <> "Backend " <> displayShow backend <> " exited with error:" <> repr n ]

-- | Bundle the project to a js file
bundleApp
  :: HasEnv env
  => WithMain
  -> Maybe ModuleName
  -> Maybe TargetPath
  -> NoBuild
  -> BuildOptions
  -> UsePsa
  -> RIO env ()
bundleApp withMain maybeModuleName maybeTargetPath noBuild buildOpts usePsa =
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      bundleAction = Purs.bundle withMain (withSourceMap buildOpts) moduleName targetPath
  in case noBuild of
    DoBuild -> Run.withBuildEnv usePsa $ build buildOpts (Just bundleAction)
    NoBuild -> Run.getEnv >>= (flip runRIO) bundleAction

-- | Bundle into a CommonJS module
bundleModule
  :: HasEnv env
  => Maybe ModuleName
  -> Maybe TargetPath
  -> NoBuild
  -> BuildOptions
  -> UsePsa
  -> RIO env ()
bundleModule maybeModuleName maybeTargetPath noBuild buildOpts usePsa = do
  logDebug "Running `bundleModule`"
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      jsExport = Text.unpack $ "\nmodule.exports = PS[\""<> unModuleName moduleName <> "\"];"
      bundleAction = do
        logInfo "Bundling first..."
        Purs.bundle WithoutMain (withSourceMap buildOpts) moduleName targetPath
        -- Here we append the CommonJS export line at the end of the bundle
        try (with
              (appendonly $ pathFromText $ unTargetPath targetPath)
              (\fileHandle -> Utf8.withHandle fileHandle (Sys.hPutStrLn fileHandle jsExport)))
          >>= \case
            Right _ -> logInfo $ display $ "Make module succeeded and output file to " <> unTargetPath targetPath
            Left (n :: SomeException) -> die [ "Make module failed: " <> repr n ]
  case noBuild of
    DoBuild -> Run.withBuildEnv usePsa $ build buildOpts (Just bundleAction)
    NoBuild -> Run.getEnv >>= (flip runRIO) bundleAction


-- | Generate docs for the `sourcePaths` and run `purescript-docs-search build-index` to patch them.
docs
  :: (HasLogFunc env, HasConfig env)
  => Maybe Purs.DocsFormat
  -> [SourcePath]
  -> Packages.DepsOnly
  -> NoSearch
  -> OpenDocs
  -> RIO env ()
docs format sourcePaths depsOnly noSearch open = do
  logDebug "Running `spago docs`"
  Config{..} <- view (the @Config)
  deps <- Packages.getProjectDeps
  logInfo "Generating documentation for the project. This might take a while..."
  Purs.docs docsFormat $ Packages.getGlobs deps depsOnly configSourcePaths <> sourcePaths

  when isHTMLFormat $ do
    when (noSearch == AddSearch) $ do
      logInfo "Making the documentation searchable..."
      writeTextFile ".spago/purescript-docs-search" Templates.docsSearch
      writeTextFile ".spago/docs-search-app.js"     Templates.docsSearchApp
      let cmd = "node .spago/purescript-docs-search build-index --package-name " <> surroundQuote name
      logDebug $ "Running `" <> display cmd <> "`"
      shell cmd empty >>= \case
        ExitSuccess   -> pure ()
        ExitFailure n -> logWarn $ "Failed while trying to make the documentation searchable: " <> repr n

    link <- linkToIndexHtml
    let linkText = "Link: " <> link
    logInfo $ display linkText

    when (open == DoOpenDocs) $ do
      logInfo "Opening in browser..."
      () <$ openLink link

  where
    docsFormat = fromMaybe Purs.Html format
    isHTMLFormat = docsFormat == Purs.Html

    linkToIndexHtml = do
      currentDir <- liftIO $ Text.pack <$> getCurrentDirectory
      return ("file://" <> currentDir <> "/generated-docs/html/index.html")

    openLink link = liftIO $ Browser.openBrowser (Text.unpack link)

-- | Start a search REPL.
search
  :: (HasPurs env, HasLogFunc env, HasConfig env)
  => RIO env ()
search = do
  Config{..} <- view (the @Config)
  deps <- Packages.getProjectDeps

  logInfo "Building module metadata..."

  Purs.compile (Packages.getGlobs deps Packages.AllSources configSourcePaths)
    [ PursArg "--codegen"
    , PursArg "docs"
    ]

  writeTextFile ".spago/purescript-docs-search" Templates.docsSearch
  let cmd = "node .spago/purescript-docs-search search --package-name " <> surroundQuote name
  logDebug $ "Running `" <> display cmd <> "`"
  viewShell $ callCommand $ Text.unpack cmd
