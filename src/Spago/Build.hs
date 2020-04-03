module Spago.Build
  ( build
  , test
  , run
  , repl
  , bundleApp
  , bundleModule
  , docs
  , search
  , showPaths
  , Watch (..)
  , NoBuild (..)
  , NoInstall (..)
  , ShareOutput (..)
  , BuildOptions (..)
  , Packages.DepsOnly (..)
  , NoSearch (..)
  , OpenDocs (..)
  , PathType (..)
  , Purs.ExtraArg (..)
  , Purs.ModuleName (..)
  , Purs.SourcePath (..)
  , Purs.TargetPath (..)
  , Purs.WithMain (..)
  , Purs.WithSrcMap (..)
  ) where

import           Spago.Prelude hiding (link)

import qualified Data.List            as List
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import           System.Directory     (getCurrentDirectory)
import           System.FilePath      (splitDirectories)
import qualified System.FilePath.Glob as Glob
import qualified System.IO            as Sys
import qualified System.IO.Temp       as Temp
import qualified System.IO.Utf8       as Utf8
import qualified Turtle
import qualified System.Process       as Process
import qualified Web.Browser          as Browser

import qualified Spago.Build.Parser   as Parse
import qualified Spago.Config         as Config
import qualified Spago.Dhall          as Dhall
import qualified Spago.FetchPackage   as Fetch
import qualified Spago.GlobalCache    as GlobalCache
import qualified Spago.Messages       as Messages
import qualified Spago.Packages       as Packages
import qualified Spago.PackageSet     as PackageSet
import qualified Spago.Purs           as Purs
import qualified Spago.Templates      as Templates
import           Spago.Types          as Types
import qualified Spago.Watch          as Watch

data Watch = Watch | BuildOnce

-- | Flag to go through with the build step
--   or skip it, in the case of 'bundleApp' and 'bundleModule'.
data NoBuild = NoBuild | DoBuild

-- | Flag to skip the automatic installation of libraries on build
data NoInstall = NoInstall | DoInstall

-- | Flag to use shared output folder if possible
data ShareOutput = ShareOutput | NoShareOutput

data BuildOptions = BuildOptions
  { cacheConfig    :: Maybe GlobalCache.CacheFlag
  , shouldWatch    :: Watch
  , shouldClear    :: Watch.ClearScreen
  , sourcePaths    :: [Purs.SourcePath]
  , withSourceMap   :: Purs.WithSrcMap
  , noInstall      :: NoInstall
  , pursArgs       :: [Purs.ExtraArg]
  , depsOnly       :: Packages.DepsOnly
  , shareOutput    :: ShareOutput
  , beforeCommands :: [Text]
  , thenCommands   :: [Text]
  , elseCommands   :: [Text]
  }

prepareBundleDefaults
  :: Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> (Purs.ModuleName, Purs.TargetPath)
prepareBundleDefaults maybeModuleName maybeTargetPath = (moduleName, targetPath)
  where
    moduleName = fromMaybe (Purs.ModuleName "Main") maybeModuleName
    targetPath = fromMaybe (Purs.TargetPath "index.js") maybeTargetPath

--   eventually running some other action after the build
build 
  :: forall env
  .  (HasLogFunc env, HasPurs env, HasJobs env, HasConfigPath env, HasGlobalCache env)
  => BuildOptions -> Maybe (RIO env ()) 
  -> RIO env ()
build buildOpts@BuildOptions{..} maybePostBuild = do
  logDebug "Running `spago build`"
  config@Config.Config{ packageSet = Types.PackageSet{..}, ..} <- Config.ensureConfigUnsafe
  deps <- Packages.getProjectDeps config
  case noInstall of
    DoInstall -> Fetch.fetchPackages cacheConfig deps packagesMinPursVersion
    NoInstall -> pure ()
  sharedOutputArgs <- case shareOutput of
    ShareOutput   -> getBuildArgsForSharedFolder buildOpts
    NoShareOutput -> pure pursArgs
  let allPsGlobs = Packages.getGlobs   deps depsOnly configSourcePaths <> sourcePaths
      allJsGlobs = Packages.getJsGlobs deps depsOnly configSourcePaths <> sourcePaths

      buildBackend globs = do
        case alternateBackend of
          Nothing ->
              Purs.compile globs sharedOutputArgs
          Just backend -> do
              when (Purs.ExtraArg "--codegen" `List.elem` pursArgs) $
                die
                  [ "Can't pass `--codegen` option to build when using a backend"
                  , "Hint: No need to pass `--codegen corefn` explicitly when using the `backend` option."
                  , "Remove the argument to solve the error"
                  ]
              Purs.compile globs $ pursArgs ++ [ Purs.ExtraArg "--codegen", Purs.ExtraArg "corefn" ]

              logDebug $ display $ "Compiling with backend \"" <> backend <> "\""
              let backendCmd = backend -- In future there will be some arguments here
              logDebug $ "Running command `" <> display backendCmd <> "`"
              shell backendCmd empty >>= \case
                ExitSuccess   -> pure ()
                ExitFailure n -> die [ "Backend " <> displayShow backend <> " exited with error:" <> repr n ]

      buildAction globs = do
        runCommands "Before" beforeCommands
        onException ( buildBackend globs ) $ runCommands "Else" elseCommands
        fromMaybe (pure ()) maybePostBuild
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
        (buildAction (wrap <$> psMatches))

  where
    runCommands :: Text -> [Text] -> RIO env ()
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

    wrap   = Purs.SourcePath . Text.pack
    unwrap = Text.unpack . Purs.unSourcePath
    removeDotSpago = filter (\glob -> ".spago" `notElem` (splitDirectories glob))
    collapse = Turtle.encodeString . Turtle.collapse . Turtle.decodeString

-- | Start a repl
repl
  :: (HasJobs env, HasLogFunc env, HasGlobalCache env, HasGlobalCache env, HasGlobalCache env, HasConfigPath env)
  => Maybe GlobalCache.CacheFlag
  -> [Types.PackageName]
  -> [Purs.SourcePath]
  -> [Purs.ExtraArg]
  -> Packages.DepsOnly
  -> RIO env ()
repl cacheFlag newPackages sourcePaths pursArgs depsOnly = do
  logDebug "Running `spago repl`"

  try Config.ensureConfigUnsafe >>= \case
    Right config@Config.Config{..} -> do
      deps <- Packages.getProjectDeps config
      let globs = Packages.getGlobs deps depsOnly configSourcePaths <> sourcePaths
      liftIO $ Purs.repl globs pursArgs
    Left (err :: SomeException) -> do
      logDebug $ display err
      cacheDir <- view globalCacheL
      Temp.withTempDirectory cacheDir "spago-repl-tmp" $ \dir -> do
        Turtle.cd (Turtle.decodeString dir)

        Packages.initProject NoForce Dhall.WithComments

        config@Config.Config{ packageSet = Types.PackageSet{..}, ..} <- Config.ensureConfigUnsafe

        let updatedConfig = Config.Config name (dependencies <> newPackages) (Config.packageSet config) alternateBackend configSourcePaths publishConfig

        deps <- Packages.getProjectDeps updatedConfig
        let globs = Packages.getGlobs deps depsOnly $ Config.configSourcePaths updatedConfig

        Fetch.fetchPackages cacheFlag deps packagesMinPursVersion

        liftIO $ Purs.repl globs pursArgs


-- | Test the project: compile and run "Test.Main"
--   (or the provided module name) with node
test 
  :: (HasLogFunc env, HasConfigPath env, HasPurs env, HasJobs env, HasGlobalCache env)
  => Maybe Purs.ModuleName -> BuildOptions -> [Purs.ExtraArg] 
  -> RIO env ()
test maybeModuleName buildOpts extraArgs = do
  let moduleName = fromMaybe (Purs.ModuleName "Test.Main") maybeModuleName
  Config.Config { alternateBackend, configSourcePaths } <- Config.ensureConfigUnsafe
  liftIO (foldMapM (Glob.glob . Text.unpack . Purs.unSourcePath) configSourcePaths) >>= \paths -> do
    results <- forM paths $ \path -> do
      content <- readFileBinary path
      pure $ Parse.checkModuleNameMatches (encodeUtf8 $ Purs.unModuleName moduleName) content
    if or results
      then do
        runBackend alternateBackend moduleName (Just "Tests succeeded.") "Tests failed: " buildOpts extraArgs
      else do
        die [ "Module '" <> (display . Purs.unModuleName) moduleName <> "' not found! Are you including it in your build?" ]


-- | Run the project: compile and run "Main"
--   (or the provided module name) with node
run 
  :: (HasLogFunc env, HasConfigPath env, HasPurs env, HasJobs env, HasGlobalCache env)
  => Maybe Purs.ModuleName -> BuildOptions -> [Purs.ExtraArg] 
  -> RIO env ()
run maybeModuleName buildOpts extraArgs = do
  Config.Config { alternateBackend } <- Config.ensureConfigUnsafe
  let moduleName = fromMaybe (Purs.ModuleName "Main") maybeModuleName
  runBackend alternateBackend moduleName Nothing "Running failed; " buildOpts extraArgs


-- | Run the project with node (or the chosen alternate backend):
--   compile and run the provided ModuleName
runBackend
  :: (HasLogFunc env, HasConfigPath env, HasPurs env, HasJobs env, HasGlobalCache env)
  => Maybe Text
  -> Purs.ModuleName
  -> Maybe Text
  -> Text
  -> BuildOptions
  -> [Purs.ExtraArg]
  -> RIO env ()
runBackend maybeBackend moduleName maybeSuccessMessage failureMessage buildOpts extraArgs = do
  logDebug $ display $ "Running with backend: " <> fromMaybe "nodejs" maybeBackend
  let postBuild = maybe (nodeAction =<< getOutputPath buildOpts) backendAction maybeBackend
  build buildOpts (Just postBuild)
  where
    nodeArgs = Text.intercalate " " $ map Purs.unExtraArg extraArgs
    nodeContents outputPath' =
         let path = fromMaybe "output" outputPath'
         in "#!/usr/bin/env node\n\n" <> "require('../" <> Text.pack path <> "/" <> Purs.unModuleName moduleName <> "').main()"
    nodeCmd = "node .spago/run.js " <> nodeArgs
    nodeAction outputPath' = do
      logDebug "Writing .spago/run.js"
      writeTextFile ".spago/run.js" (nodeContents outputPath')
      void $ chmod executable ".spago/run.js"
      -- We build a process by hand here because we need to forward the stdin to the backend process
      let processWithStdin = (Process.shell (Text.unpack nodeCmd)) { Process.std_in = Process.Inherit }
      Turtle.system processWithStdin empty >>= \case
        ExitSuccess   -> maybe (pure ()) (logInfo . display) maybeSuccessMessage
        ExitFailure n -> die [ display failureMessage <> "exit code: " <> repr n ]
    backendAction backend =
      Turtle.proc backend (["--run" {-, Purs.unModuleName moduleName-}] <> fmap Purs.unExtraArg extraArgs) empty >>= \case
        ExitSuccess   -> maybe (pure ()) (logInfo . display) maybeSuccessMessage
        ExitFailure n -> die [ display failureMessage <> "Backend " <> displayShow backend <> " exited with error:" <> repr n ]

-- | Bundle the project to a js file
bundleApp
  :: (HasLogFunc env, HasPurs env, HasJobs env, HasConfigPath env, HasGlobalCache env)
  => Purs.WithMain
  -> Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> NoBuild
  -> BuildOptions
  -> RIO env ()
bundleApp withMain maybeModuleName maybeTargetPath noBuild buildOpts =
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      bundleAction = Purs.bundle withMain (withSourceMap buildOpts) moduleName targetPath
  in case noBuild of
    DoBuild -> build buildOpts (Just bundleAction)
    NoBuild -> bundleAction

-- | Bundle into a CommonJS module
bundleModule
  :: (HasLogFunc env, HasPurs env, HasJobs env, HasConfigPath env, HasGlobalCache env)
  => Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> NoBuild
  -> BuildOptions
  -> RIO env ()
bundleModule maybeModuleName maybeTargetPath noBuild buildOpts = do
  logDebug "Running `bundleModule`"
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      jsExport = Text.unpack $ "\nmodule.exports = PS[\""<> Purs.unModuleName moduleName <> "\"];"
      bundleAction = do
        logInfo "Bundling first..."
        Purs.bundle Purs.WithoutMain (withSourceMap buildOpts) moduleName targetPath
        -- Here we append the CommonJS export line at the end of the bundle
        try (with
              (appendonly $ pathFromText $ Purs.unTargetPath targetPath)
              (\fileHandle -> Utf8.withHandle fileHandle (Sys.hPutStrLn fileHandle jsExport)))
          >>= \case
            Right _ -> logInfo $ display $ "Make module succeeded and output file to " <> Purs.unTargetPath targetPath
            Left (n :: SomeException) -> die [ "Make module failed: " <> repr n ]
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
  :: (HasLogFunc env, HasConfigPath env)
  => Maybe Purs.DocsFormat
  -> [Purs.SourcePath]
  -> Packages.DepsOnly
  -> NoSearch
  -> OpenDocs
  -> RIO env ()
docs format sourcePaths depsOnly noSearch open = do
  logDebug "Running `spago docs`"
  config@Config.Config{..} <- Config.ensureConfigUnsafe
  deps <- Packages.getProjectDeps config
  logInfo "Generating documentation for the project. This might take a while..."
  Purs.docs docsFormat $ Packages.getGlobs deps depsOnly configSourcePaths <> sourcePaths

  when isHTMLFormat $ do
    when (noSearch == AddSearch) $ do
      logInfo "Making the documentation searchable..."
      writeTextFile ".spago/purescript-docs-search" Templates.docsSearch
      writeTextFile ".spago/docs-search-app.js"     Templates.docsSearchApp
      let cmd = "node .spago/purescript-docs-search build-index"
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
  :: (HasPurs env, HasLogFunc env, HasConfigPath env) 
  => RIO env ()
search = do
  config@Config.Config{..} <- Config.ensureConfigUnsafe
  deps <- Packages.getProjectDeps config

  logInfo "Building module metadata..."

  Purs.compile (Packages.getGlobs deps Packages.AllSources configSourcePaths)
    [ Purs.ExtraArg "--codegen"
    , Purs.ExtraArg "docs"
    ]

  writeTextFile ".spago/purescript-docs-search" Templates.docsSearch
  let cmd = "node .spago/purescript-docs-search search"
  logDebug $ "Running `" <> display cmd <> "`"
  viewShell $ callCommand $ Text.unpack cmd


-- | Find the output path for purs compiler
-- | This is based on the location of packages.dhall, the shareOutput flag
-- | and whether the user has manually specified a path in pursArgs
getOutputPath
  :: (HasConfigPath env, HasLogFunc env)
  => BuildOptions
  -> RIO env (Maybe Sys.FilePath)
getOutputPath buildOpts = do
  configPath <- view configPathL
  outputPath <- PackageSet.findRootOutputPath (Text.unpack configPath)
  case findOutputFlag (pursArgs buildOpts) of
    Just path -> pure (Just path)
    Nothing   ->
      case shareOutput buildOpts of
        NoShareOutput -> pure Nothing
        ShareOutput   -> pure outputPath

getOutputPathOrDefault
  :: (HasConfigPath env, HasLogFunc env)
  => BuildOptions
  -> RIO env Sys.FilePath
getOutputPathOrDefault buildOpts
  = (fromMaybe "output") <$> getOutputPath buildOpts

data PathType
  = OutputFolder

-- | Used by `spago path output` command
showOutputPath
  :: (HasConfigPath env, HasLogFunc env)
  => BuildOptions
  -> RIO env ()
showOutputPath buildOptions =
  outputStr =<< getOutputPathOrDefault buildOptions

showPaths
  :: (HasLogFunc env, HasConfigPath env)
  => BuildOptions
  -> Maybe PathType
  -> RIO env ()
showPaths buildOptions whichPaths =
  case whichPaths of
    (Just OutputFolder) -> showOutputPath buildOptions
    Nothing             -> showAllPaths buildOptions

showAllPaths
  :: (HasConfigPath env, HasLogFunc env)
  => BuildOptions
  -> RIO env ()
showAllPaths buildOptions =
  traverse_ showPath =<< getAllPaths buildOptions
  where
    showPath (a,b)
      = output (a <> ": " <> b)

getAllPaths
  :: (HasConfigPath env, HasLogFunc env)
  => BuildOptions
  -> RIO env [(Text, Text)]
getAllPaths buildOptions = do
  outputPath <- getOutputPathOrDefault buildOptions
  pure [ ("output", Text.pack outputPath) ]

-- | Find an output flag and then return the next item
-- | which should be the output folder
findOutputFlag :: [Purs.ExtraArg] -> Maybe Sys.FilePath
findOutputFlag [] = Nothing
findOutputFlag [_] = Nothing
findOutputFlag (x:y:xs)
  = if isOutputFlag x
    then Just $ Text.unpack (Purs.unExtraArg y)
    else findOutputFlag (y : xs)

-- | is this argument specifying an output folder?
isOutputFlag :: Purs.ExtraArg -> Bool
isOutputFlag (Purs.ExtraArg a)
  =  firstWord == "-o"
  || firstWord == "--output"
    where
      firstWord
        = fromMaybe "" $ case Text.words a of
             []       -> Nothing
             (word:_) -> Just word

-- | If we aren't using the --no-share-output flag, calculate the extra args to
-- | send to Purs compile
getBuildArgsForSharedFolder
  :: (HasConfigPath env, HasLogFunc env)
  => BuildOptions
  -> RIO env [Purs.ExtraArg]
getBuildArgsForSharedFolder buildOpts = do
  let pursArgs'
        = pursArgs buildOpts
      pathToOutputArg
        = Purs.ExtraArg . Text.pack . ("--output " <>)
  if any isOutputFlag pursArgs'
    then do
      logInfo "Output path set explicitly - not using shared output path"
      pure pursArgs'
    else do
      outputFolder <- getOutputPath buildOpts
      case pathToOutputArg <$> outputFolder of
        Just newArg -> pure (pursArgs' <> [newArg])
        _           -> pure pursArgs'
