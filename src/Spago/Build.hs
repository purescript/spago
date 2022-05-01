module Spago.Build
  ( build
  , test
  , run
  , repl
  , bundleModule
  , docs
  , search
  , script
  ) where

import           Spago.Prelude hiding (link)
import           Spago.Env

import qualified Crypto.Hash          as Hash
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.Versions        as Version
import           System.Directory     (getCurrentDirectory)
import           System.FilePath      (splitDirectories)
import qualified System.FilePath.Glob as Glob
import qualified System.IO            as Sys
import qualified System.IO.Temp       as Temp
import qualified System.IO.Utf8       as Utf8
import qualified Turtle
import qualified System.Process       as Process
import qualified Web.Browser          as Browser

import qualified Spago.Command.Path   as Path
import qualified Spago.RunEnv         as Run
import qualified Spago.Config         as Config
import qualified Spago.FetchPackage   as Fetch
import qualified Spago.Messages       as Messages
import qualified Spago.Packages       as Packages
import qualified Spago.Purs           as Purs
import qualified Spago.Templates      as Templates
import qualified Spago.Watch          as Watch
import qualified Spago.Cmd            as Cmd


prepareBundleDefaults
  :: Maybe ModuleName
  -> Maybe TargetPath
  -> Maybe Platform
  -> (ModuleName, TargetPath, Platform)
prepareBundleDefaults maybeModuleName maybeTargetPath maybePlatform = (moduleName, targetPath, platform)
  where
    moduleName = fromMaybe (ModuleName "Main") maybeModuleName
    targetPath = fromMaybe (TargetPath "index.js") maybeTargetPath
    platform = fromMaybe Browser maybePlatform

--   eventually running some other action after the build
build :: HasBuildEnv env => Maybe (RIO env ()) -> RIO env ()
build maybePostBuild = do
  logDebug "Running `spago build`"
  BuildOptions{..} <- view (the @BuildOptions)
  Config{..} <- view (the @Config)
  deps <- Packages.getProjectDeps
  let partitionedGlobs@(Packages.Globs{..}) = Packages.getGlobs deps depsOnly (toList configSourcePaths)
      allPsGlobs = Packages.getGlobsSourcePaths partitionedGlobs <> sourcePaths
      allJsGlobs = Packages.getJsGlobs deps depsOnly (toList configSourcePaths) <> sourcePaths

      checkImports = do
        maybeGraph <- view (the @Graph)
        for_ maybeGraph $ \(Purs.ModuleGraph moduleGraph) -> do
              let
                matchesGlob :: Sys.FilePath -> SourcePath -> Bool
                matchesGlob path sourcePath =
                  Glob.match (Glob.compile (Text.unpack (unSourcePath sourcePath))) path

                isProjectFile :: Sys.FilePath -> Bool
                isProjectFile path =
                  any (matchesGlob path) (fromMaybe [] projectGlobs)

                projectModules :: [ModuleName]
                projectModules =
                  map fst
                    $ filter (\(_, Purs.ModuleGraphNode{..}) -> isProjectFile (Text.unpack graphNodePath))
                    $ Map.toList moduleGraph

                getImports :: ModuleName -> Set ModuleName
                getImports = maybe Set.empty (Set.fromList . graphNodeDepends) . flip Map.lookup moduleGraph

                -- All package modules that are imported from our project files
                importedPackageModules :: Set ModuleName
                importedPackageModules =
                  Set.difference
                    (foldMap getImports projectModules)
                    (Set.fromList projectModules)

                getPackageFromPath :: Text -> Maybe PackageName
                getPackageFromPath path =
                  fmap fst
                    $ find (\(_, sourcePath) -> matchesGlob (Text.unpack path) sourcePath)
                    $ Map.toList depsGlobs

                importedPackages :: Set PackageName
                importedPackages =
                  Set.fromList
                    $ mapMaybe (getPackageFromPath . graphNodePath <=< flip Map.lookup moduleGraph)
                    $ Set.toList importedPackageModules

                dependencyPackages :: Set PackageName
                dependencyPackages = dependencies

              let
                unusedPackages =
                  fmap packageName
                    $ Set.toList
                    $ Set.difference dependencyPackages importedPackages

                transitivePackages =
                  fmap packageName
                    $ Set.toList
                    $ Set.difference importedPackages dependencyPackages

              unless (null unusedPackages || depsOnly == DepsOnly) $ do
                logWarn $ display $ Messages.unusedDependency unusedPackages

              unless (null transitivePackages) $ do
                die [ display $ Messages.sourceImportsTransitiveDependency transitivePackages ]


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
        checkImports

      buildAction globs = do
        let action = buildBackend globs >> (fromMaybe (pure ()) maybePostBuild)
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
  -> PackageName
  -> RIO env ()
repl newPackages sourcePaths pursArgs depsOnly replPackage = do
  logDebug "Running `spago repl`"
  purs <- Run.getPurs NoPsa
  Config.ensureConfig >>= \case
    Right config -> Run.withInstallEnv' (Just (ensurePsciSupportIncluded config)) (replAction purs)
    Left err -> do
      logDebug err
      GlobalCache cacheDir _ <- view (the @GlobalCache)
      Temp.withTempDirectory cacheDir "spago-repl-tmp" $ \dir -> do
        Turtle.cd (Turtle.decodeString dir)

        writeTextFile ".purs-repl" Templates.pursRepl

        let dependencies = Set.fromList $ [ PackageName "effect", PackageName "console" ] <> newPackages

        config <- Run.withPursEnv NoPsa $ do
          Config.makeTempConfig dependencies Nothing Set.empty Nothing

        Run.withInstallEnv' (Just (ensurePsciSupportIncluded config)) (replAction purs)
  where
    ensurePsciSupportIncluded :: Config -> Config
    ensurePsciSupportIncluded originalConfig@Config{dependencies = originalDeps}
      | replPackage `elem` originalDeps = originalConfig
      | otherwise = originalConfig { dependencies = Set.insert replPackage originalDeps }

    replAction purs = do
      Config{..} <- view (the @Config)
      deps <- Packages.getProjectDeps
      let
        globs =
          Packages.getGlobsSourcePaths $ Packages.getGlobs deps depsOnly (toList $ configSourcePaths <> Set.fromList sourcePaths)
      Fetch.fetchPackages deps
      runRIO purs $ Purs.repl globs pursArgs


-- | Test the project: compile and run "Test.Main"
--   (or the provided module name) with node
test :: HasBuildEnv env => Maybe ModuleName -> [BackendArg] -> RIO env ()
test maybeModuleName extraArgs = do
  logDebug "Running `Spago.Build.test`"
  let moduleName = fromMaybe (ModuleName "Test.Main") maybeModuleName
  Config.Config { alternateBackend } <- view (the @Config)

  -- We check if the test module is included in the build and spit out a nice error if it isn't (see #383)
  maybeGraph <- view (the @Graph)
  for_ maybeGraph $ \(ModuleGraph moduleMap) -> when (isNothing $ Map.lookup moduleName moduleMap) $
    die [ "Module '" <> (display . unModuleName) moduleName <> "' not found! Are you including it in your build?" ]

  sourceDir <- Turtle.pwd
  let dirs = RunDirectories sourceDir sourceDir
  runBackend alternateBackend dirs moduleName (Just "Tests succeeded.") "Tests failed: " extraArgs


-- | Run the project: compile and run "Main"
--   (or the provided module name) with node
run :: HasBuildEnv env => Maybe ModuleName -> [BackendArg] -> RIO env ()
run maybeModuleName extraArgs = do
  Config.Config { alternateBackend } <- view (the @Config)
  let moduleName = fromMaybe (ModuleName "Main") maybeModuleName
  sourceDir <- Turtle.pwd
  let dirs = RunDirectories sourceDir sourceDir
  runBackend alternateBackend dirs moduleName Nothing "Running failed; " extraArgs


-- | Run the select module as a script: init, compile, and run the provided module
script
  :: (HasEnv env)
  => Text
  -> Maybe Text
  -> [PackageName]
  -> ScriptBuildOptions
  -> RIO env ()
script modulePath tag packageDeps opts = do
  logDebug "Running `spago script`"
  absoluteModulePath <- fmap Text.pack (makeAbsolute (Text.unpack modulePath))
  currentDir <- Turtle.pwd

  -- This is the part where we make sure that the script reuses the same folder
  -- if run with the same options more than once. We do that by making a folder
  -- in the system temp directory, and naming it with the hash of the script
  -- path together with the command options
  let sha256 :: String -> String
      sha256 = show . (Hash.hash :: ByteString -> Hash.Digest Hash.SHA256) . Turtle.fromString
  systemTemp <- liftIO $ Temp.getCanonicalTemporaryDirectory
  let stableHash = sha256 (Text.unpack absoluteModulePath <> show tag <> show packageDeps <> show opts)
  let scriptDirPath = Turtle.decodeString (systemTemp </> "spago-script-tmp-" <> stableHash)
  logDebug $ "Found a system temp directory: " <> displayShow systemTemp

  -- We now create and cd into this new temp directory
  logWarn $ "Creating semi-temp directory to run the script: " <> displayShow scriptDirPath
  Turtle.mktree scriptDirPath
  Turtle.cd scriptDirPath

  let dependencies = Set.fromList $ [ PackageName "effect", PackageName "console", PackageName "prelude" ] <> packageDeps

  config <- Run.withPursEnv NoPsa $ do
    Config.makeTempConfig dependencies Nothing (Set.fromList [ SourcePath absoluteModulePath ]) tag

  let runDirs :: RunDirectories
      runDirs = RunDirectories scriptDirPath currentDir

  Run.withBuildEnv' (Just config) NoPsa buildOpts (runAction runDirs)
  where
    buildOpts = fromScriptOptions defaultBuildOptions opts
    runAction dirs = runBackend Nothing dirs (ModuleName "Main") Nothing "Script failed to run; " []


data RunDirectories = RunDirectories { sourceDir :: FilePath, executeDir :: FilePath }

data NodeEsSupport = Unsupported Version.SemVer | Experimental | Supported

hasNodeEsSupport :: (HasLogFunc env) => RIO env NodeEsSupport
hasNodeEsSupport = do
  nodeVersion <- Cmd.getCmdVersion "node"
  case nodeVersion  of
    Left err -> do
      logDebug $ display $ "Unable to get Node.js version: " <> displayShow err
      pure Supported
    Right nv@Version.SemVer{} | Version._svMajor nv < 12 ->
      pure $ Unsupported nv
    Right nv@Version.SemVer{} | Version._svMajor nv >= 12 && Version._svMajor nv < 13 ->
      pure Experimental
    _ -> pure Supported


-- | Run the project with node (or the chosen alternate backend):
--   compile and run the provided ModuleName
runBackend
  :: HasBuildEnv env
  => Maybe Text
  -> RunDirectories
  -> ModuleName
  -> Maybe Text
  -> Text
  -> [BackendArg]
  -> RIO env ()
runBackend maybeBackend RunDirectories{ sourceDir, executeDir } moduleName maybeSuccessMessage failureMessage extraArgs = do
  logDebug $ display $ "Running with backend: " <> fromMaybe "nodejs" maybeBackend
  BuildOptions{ pursArgs } <- view (the @BuildOptions)
  let
    postBuild = maybe (nodeAction $ Path.getOutputPath pursArgs) backendAction maybeBackend
  build (Just postBuild)
  where
    fromFilePath = Text.pack . Turtle.encodeString
    runJsSource = fromFilePath (sourceDir Turtle.</> ".spago/run.js")
    packageJson = fromFilePath (sourceDir Turtle.</> ".spago/package.json")
    nodeArgs = Text.intercalate " " $ map unBackendArg extraArgs
    esContents outputPath' =
      fold
        [ "import { main } from 'file://"
        , Text.replace "\\" "/" (fromFilePath sourceDir)
        , "/"
        , Text.pack outputPath'
        , "/"
        , unModuleName moduleName
        , "/"
        , "index.js"
        , "'\n\n"
        , "main()"
        ]
    cjsContents outputPath' =
      fold
        [ "require('"
        , Text.replace "\\" "/" (fromFilePath sourceDir)
        , "/"
        , Text.pack outputPath'
        , "/"
        , unModuleName moduleName
        , "').main()"
        ]
    nodeContents isES outputPath' =
      if isES then esContents outputPath'
      else cjsContents outputPath'

    packageJsonContents = "{\"type\":\"module\" }"

    nodeCmd isES Experimental | isES = "node --experimental-modules \"" <> runJsSource <> "\" " <> nodeArgs
    nodeCmd _ _ = "node \"" <> runJsSource <> "\" " <> nodeArgs

    nodeAction outputPath' = do
      isES <- Purs.hasMinPursVersion "0.15.0-alpha-01"
      nodeVersion <- hasNodeEsSupport
      case (isES, nodeVersion) of
        (True, Unsupported nv) ->
          die [ "Unsupported Node.js version: " <> display (Version.prettySemVer nv), "Required Node.js version >=12." ]
        _ ->
          pure ()
      logDebug $ "Writing " <> displayShow @Text runJsSource
      writeTextFile runJsSource (nodeContents isES outputPath')
      void $ chmod executable $ pathFromText runJsSource
      if isES then do
        logDebug $ "Writing " <> displayShow @Text packageJson
        writeTextFile packageJson packageJsonContents
      else pure ()
      -- cd to executeDir in case it isn't the same as sourceDir
      logDebug $ "Executing from: " <> displayShow @FilePath executeDir
      Turtle.cd executeDir
      -- We build a process by hand here because we need to forward the stdin to the backend process
      logDebug $ "Running node command: `" <> display (nodeCmd isES nodeVersion) <> "`"
      let processWithStdin = (Process.shell (Text.unpack $ nodeCmd isES nodeVersion)) { Process.std_in = Process.Inherit }
      Turtle.system processWithStdin empty >>= \case
        ExitSuccess   -> maybe (pure ()) (logInfo . display) maybeSuccessMessage
        ExitFailure n -> die [ display failureMessage <> "exit code: " <> repr n ]
    backendAction backend = do
      let args :: [Text] = ["--run", unModuleName moduleName <> ".main"] <> fmap unBackendArg extraArgs
      logDebug $ display $ "Running command `" <> backend <> " " <> Text.unwords args <> "`"
      Turtle.proc backend args empty >>= \case
        ExitSuccess   -> maybe (pure ()) (logInfo . display) maybeSuccessMessage
        ExitFailure n -> die [ display failureMessage <> "Backend " <> displayShow backend <> " exited with error:" <> repr n ]


bundleWithEsbuild :: HasLogFunc env => WithMain -> WithSrcMap -> ModuleName -> TargetPath -> Platform -> Minify -> RIO env ()
bundleWithEsbuild withMain srcMap (ModuleName moduleName) (TargetPath targetPath) platform minify = do
  esbuild <- getESBuild
  let
    platformOpt = case platform of
      Browser -> ["--platform=browser"]
      Node -> ["--platform=node"]
    minifyOpt = case minify of
      NoMinify -> []
      Minify -> ["--minify"]
    srcMapOpt = case srcMap of
      WithSrcMap -> ["--sourcemap"]
      WithoutSrcMap -> []
    formatOpt = case (platform, withMain) of
      (Browser, WithMain) -> ["--format=iife"]
      (_,_) -> ["--format=esm"]
    esbuildBase = platformOpt <> minifyOpt <> srcMapOpt <> formatOpt <> ["--bundle", "--outfile=" <> targetPath]
    (input, cmd) = case withMain of
      WithMain -> do
        let
          echoLine = "import { main } from './output/" <> moduleName <> "/index.js'; main();"
        (Turtle.textToLine echoLine, esbuild :| esbuildBase)
      WithoutMain ->
        (Nothing, esbuild :| esbuildBase <> ["output/" <> moduleName <> "/index.js"])
  runProcessWithOutput cmd input
    ("Bundle succeeded and output file to " <> targetPath)
    "Bundle failed."
  where
  getESBuild = do
    maybeESBuild <- findExecutable "esbuild"
    case maybeESBuild of
      Nothing -> die [ "Failed to find esbuild. See https://esbuild.github.io/getting-started/#install-esbuild for ways to install esbuild." ]
      Just esBuild -> pure esBuild

-- | Bundle the project to a CommonJs module/app or an ES module
bundleModule
  :: (HasEnv env, HasPurs env)
  => WithMain
  -> BundleOptions
  -> BuildOptions
  -> UsePsa
  -> RIO env ()
bundleModule withMain BundleOptions { maybeModuleName, maybeTargetPath, maybePlatform, minify, noBuild }  buildOpts usePsa = do
  isES <- Purs.hasMinPursVersion "0.15.0-alpha-01"
  let
    (moduleName, targetPath, platform) = prepareBundleDefaults maybeModuleName maybeTargetPath maybePlatform
    bundleAction =
      if isES then
        bundleWithEsbuild withMain (withSourceMap buildOpts) moduleName targetPath platform minify
      else case withMain of
        WithMain -> Purs.bundle WithMain (withSourceMap buildOpts) moduleName targetPath
        WithoutMain ->
          let
            jsExport = Text.unpack $ "\nmodule.exports = PS[\""<> unModuleName moduleName <> "\"];"
          in do
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
    DoBuild -> Run.withBuildEnv usePsa buildOpts $ build (Just bundleAction)
    NoBuild -> Run.withBuildEnv usePsa buildOpts bundleAction

docsSearchTemplate :: (HasType LogFunc env, HasType PursCmd env) => RIO env Text
docsSearchTemplate = ifM (Purs.hasMinPursVersion "0.14.0")
  (pure Templates.docsSearch0011)
  (pure Templates.docsSearch0010)

docsSearchAppTemplate :: (HasType LogFunc env, HasType PursCmd env) => RIO env Text
docsSearchAppTemplate = ifM (Purs.hasMinPursVersion "0.14.0")
  (pure Templates.docsSearchApp0011)
  (pure Templates.docsSearchApp0010)

-- | Generate docs for the `sourcePaths` and run `purescript-docs-search build-index` to patch them.
docs
  :: HasBuildEnv env
  => Maybe Purs.DocsFormat
  -> NoSearch
  -> OpenDocs
  -> RIO env ()
docs format noSearch open = do
  logDebug "Running `spago docs`"
  BuildOptions { sourcePaths, depsOnly } <- view (the @BuildOptions)
  Config{..} <- view (the @Config)
  deps <- Packages.getProjectDeps
  logInfo "Generating documentation for the project. This might take a while..."
  Purs.docs docsFormat $ Packages.getGlobsSourcePaths (Packages.getGlobs deps depsOnly (toList configSourcePaths)) <> sourcePaths

  when isHTMLFormat $ do
    when (noSearch == AddSearch) $ do
      logInfo "Making the documentation searchable..."
      writeTextFile ".spago/purescript-docs-search" =<< docsSearchTemplate
      writeTextFile ".spago/docs-search-app.js" =<< docsSearchAppTemplate
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
search :: HasBuildEnv env => RIO env ()
search = do
  Config{..} <- view (the @Config)
  deps <- Packages.getProjectDeps

  logInfo "Building module metadata..."

  Purs.compile (Packages.getGlobsSourcePaths (Packages.getGlobs deps Packages.AllSources (toList configSourcePaths)))
    [ PursArg "--codegen"
    , PursArg "docs"
    ]

  writeTextFile ".spago/purescript-docs-search" =<< docsSearchTemplate
  let cmd = "node .spago/purescript-docs-search search --package-name " <> surroundQuote name
  logDebug $ "Running `" <> display cmd <> "`"
  viewShell $ callCommand $ Text.unpack cmd
