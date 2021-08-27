{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Spago.Config
  ( defaultPath
  , makeConfig
  , makeTempConfig
  , ensureConfig
  , addDependencies
  , parsePackage
  , parsePackageSet
  , Config(..)
  , PublishConfig(..)
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Data.List             as List
import qualified Data.List.NonEmpty    as NonEmpty
import qualified Data.Map              as Map
import qualified Data.SemVer           as SemVer
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Versions         as Version
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.TypeCheck
import qualified Web.Bower.PackageMeta as Bower

import qualified Spago.Dhall           as Dhall
import qualified Spago.Messages        as Messages
import qualified Spago.PackageSet      as PackageSet
import qualified Spago.PscPackage      as PscPackage
import qualified Spago.Targets         as Targets
import qualified Spago.Templates       as Templates


type Expr = Dhall.DhallExpr Dhall.Import
type ResolvedExpr = Dhall.DhallExpr Void


-- | Default path for the Spago Config
defaultPath :: IsString t => t
defaultPath = "spago.dhall"


isLocationType :: (Eq s, Eq a) => Dhall.Expr s a -> Bool
isLocationType (Dhall.Union kvs) | locationUnionMap == Dhall.Map.toMap kvs = True
  where
    locationUnionMap = Map.fromList
      [ ("Environment", Just Dhall.Text)
      , ("Remote", Just Dhall.Text)
      , ("Local", Just Dhall.Text)
      , ("Missing", Nothing)
      ]
isLocationType _ = False


dependenciesType :: Dhall.Decoder [PackageName]
dependenciesType = Dhall.list (Dhall.auto :: Dhall.Decoder PackageName)


sourcesType :: Dhall.Decoder [SourcePath]
sourcesType  = Dhall.list (Dhall.auto :: Dhall.Decoder SourcePath)

parsePackage :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env) => ResolvedExpr -> m Package
parsePackage (Dhall.RecordLit ks') = do
  let ks = Dhall.extractRecordValues ks'
  repo         <- Dhall.requireTypedKey ks "repo" (Dhall.auto :: Dhall.Decoder Repo)
  version      <- Dhall.requireTypedKey ks "version" Dhall.strictText
  dependencies <- Dhall.requireTypedKey ks "dependencies" dependenciesType
  let location = Remote{..}
  pure Package{..}
parsePackage (Dhall.App
               (Dhall.Field union (Dhall.FieldSelection { fieldSelectionLabel = "Local" }))
               (Dhall.TextLit (Dhall.Chunks [] spagoConfigPath)))
  | isLocationType union = do
      localPath <- case Text.isSuffixOf "/spago.dhall" spagoConfigPath of
        True  -> pure $ Text.dropEnd 12 spagoConfigPath
        False -> die [ display $ Messages.failedToParseLocalRepo spagoConfigPath ]
      rawConfig <- liftIO $ Dhall.readRawExpr spagoConfigPath
      dependencies <- case rawConfig of
        Nothing -> die [ display $ Messages.cannotFindConfigLocalPackage spagoConfigPath ]
        Just (_header, expr) -> do
          newExpr <- transformMExpr (pure . filterDependencies . addSourcePaths) expr
          -- Note: we have to use inputWithSettings here because we're about to resolve
          -- the raw config from the local project. So if that has any imports they
          -- should be relative to the directory of that package
          liftIO $
            Dhall.inputWithSettings
              (set Dhall.rootDirectory (Text.unpack localPath) Dhall.defaultInputSettings)
              dependenciesType
              (pretty newExpr)
      let location = Local{..}
      pure Package{..}
parsePackage expr = die [ display $ Messages.failedToParsePackage $ pretty expr ]

parseTarget :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env) => ResolvedExpr -> m Target
parseTarget (Dhall.RecordLit ks') = do
  let ks = Dhall.extractRecordValues ks'
  targetDependencies <- Dhall.requireTypedKey ks "dependencies" dependenciesType
  targetSourcePaths <- Dhall.requireTypedKey ks "sources" sourcesType
  pure Target{..}
parseTarget expr = die [ display $ Messages.failedToParseTarget $ pretty expr ]

-- | Parse the contents of a "packages.dhall" file (or the "packages" key of an
-- evaluated "spago.dhall")
parsePackageSet 
  :: HasLogFunc env
  => Dhall.Map.Map Text (Dhall.DhallExpr Void) 
  -> RIO env PackageSet
parsePackageSet pkgs = do
  packagesDB <- fmap (Map.mapKeys PackageName . Dhall.Map.toMap) $ traverse parsePackage pkgs

  let metadataPackageName = PackageName "metadata"
  let packagesMinPursVersion = join
        $ fmap (hush . Version.semver . Text.replace "v" "" . version . location)
        $ Map.lookup metadataPackageName packagesDB
  pure PackageSet{..}


-- | Parse the contents of the "targets" key of an evaluated "spago.dhall")
parseTargets
  :: HasLogFunc env
  => Dhall.Map.Map Text (Dhall.DhallExpr Void)
  -> RIO env (Map TargetName Target)
parseTargets config = do
  fmap (Map.mapKeys TargetName . Dhall.Map.toMap) $ traverse parseTarget config

-- | Tries to read in a Spago Config
parseConfig
  :: (HasLogFunc env, HasConfigPath env)
  => RIO env Config
parseConfig = do
  -- Here we try to migrate any config that is not in the latest format
  void $ withConfigAST $ pure . addSourcePaths

  ConfigPath path <- view (the @ConfigPath)
  expr <- liftIO $ Dhall.inputExpr $ "./" <> path
  case Dhall.normalize expr of
    Dhall.RecordLit ks' -> do
      let ks = Dhall.extractRecordValues ks'
      name              <- Dhall.requireTypedKey ks "name" Dhall.strictText
      targets           <- Dhall.requireKey ks "targets" (\case
        Dhall.RecordLit tgts -> parseTargets (Dhall.extractRecordValues tgts)
        something            -> throwM $ Dhall.TargetsIsNotRecord something)
      alternateBackend  <- Dhall.maybeTypedKey ks "backend" Dhall.strictText

      let ensurePublishConfig = do
            publishLicense    <- Dhall.requireTypedKey ks "license" Dhall.strictText
            publishRepository <- Dhall.requireTypedKey ks "repository" Dhall.strictText
            pure PublishConfig{..}
      publishConfig <- try ensurePublishConfig

      packageSet <- Dhall.requireKey ks "packages" (\case
        Dhall.RecordLit pkgs -> parsePackageSet (Dhall.extractRecordValues pkgs)
        something            -> throwM $ Dhall.PackagesIsNotRecord something)

      pure Config{..}
    _ -> case Dhall.TypeCheck.typeOf $ Dhall.normalize expr of
      Right e  -> throwM $ Dhall.ConfigIsNotRecord e
      Left err -> throwM err


-- | Checks that the Spago config is there and readable
ensureConfig
  :: (HasLogFunc env, HasConfigPath env)
  => RIO env (Either Utf8Builder Config)
ensureConfig = do
  ConfigPath path <- view (the @ConfigPath)
  exists <- testfile path
  if not exists
    then pure $ Left $ display $ Messages.cannotFindConfig path
    else try parseConfig >>= \case
      Right config -> do
        PackageSet.ensureFrozen $ Text.unpack path
        pure $ Right config
      Left (err :: Dhall.ReadError Void) -> pure $ Left $ displayShow err

-- | Create a Config in memory
-- | For use by `spago script` and `spago repl`
makeTempConfig
  :: (HasLogFunc env, HasPurs env)
  => [PackageName]
  -> Maybe Text
  -> [SourcePath]
  -> Maybe Text
  -> RIO env Config
makeTempConfig dependencies alternateBackend targetSourcePaths maybeTag = do
  PursCmd { compilerVersion } <- view (the @PursCmd)
  let targets = Map.singleton Targets.mainTarget (Target { targetDependencies = dependencies, targetSourcePaths = targetSourcePaths })
  tag <- case maybeTag of
    Nothing ->
      PackageSet.getLatestSetForCompilerVersion compilerVersion "purescript" "package-sets" >>= \case
        Left _ -> die [ "spago script: failed to fetch latest package set tag" ]
        Right tag -> pure tag
    Just tag -> pure tag

  expr <- liftIO $ Dhall.inputExpr $ "https://github.com/purescript/package-sets/releases/download/" <> tag <> "/packages.dhall"

  case expr of
    Dhall.RecordLit ks' -> do
      let ks = Dhall.extractRecordValues ks'
      packageSet <- parsePackageSet ks
      let publishConfig = Left $ Dhall.RequiredKeyMissing "license" ks
      pure $ Config { name = "", ..}
    _ -> die [ "Failed to parse package set" ]

-- | Copies over `spago.dhall` to set up a Spago project.
--   Eventually ports an existing `psc-package.json` to the new config.
makeConfig
  :: (HasConfigPath env, HasLogFunc env)
  => Force -> Dhall.TemplateComments
  -> RIO env Config
makeConfig force comments = do
  ConfigPath path <- view (the @ConfigPath)
  when (force == NoForce) $ do
    hasSpagoDhall <- testfile path
    when hasSpagoDhall $ die [ display $ Messages.foundExistingProject path ]
  writeTextFile path $ Dhall.processComments comments Templates.spagoDhall
  Dhall.format path

  -- We try to find an existing psc-package or Bower config, and if
  -- we find any we migrate the existing content
  -- Otherwise we just keep the default template
  bowerFileExists <- testfile "bower.json"
  pscfileExists <- testfile PscPackage.configPath
  eitherConfig <- ensureConfig

  case (pscfileExists, bowerFileExists) of
    (True, _) -> do
      -- first, read the psc-package file content
      content <- readTextFile PscPackage.configPath
      case (eitherDecodeStrict $ Text.encodeUtf8 content, eitherConfig) of
        (Left err, _) -> logWarn $ display $ Messages.failedToReadPscFile err
        (_, Left err) -> die [err]
        (Right pscConfig, Right config) -> do
          logInfo "Found a \"psc-package.json\" file, migrating to a new Spago config.."
          -- try to update the dependencies (will fail if not found in package set)
          let pscPackages = map PackageName $ PscPackage.depends pscConfig
          void $ withConfigAST ( addRawDeps config Targets.mainTarget pscPackages
                               . updateName (PscPackage.name pscConfig))
    (_, True) -> do
      -- read the bowerfile
      content <- readTextFile "bower.json"
      case (eitherDecodeStrict $ Text.encodeUtf8 content, eitherConfig) of
        (Left err, _) -> logWarn $ display $ Messages.failedToParseFile path err
        (_, Left err) -> die [err]
        (Right packageMeta, Right config@Config{..}) -> do
          logInfo "Found a \"bower.json\" file, migrating to a new Spago config.."
          -- then try to update the dependencies. We'll migrates the ones that we can,
          -- and print a message to the user to fix the missing ones
          let (bowerName, deps, devDeps) = migrateBower packageMeta packageSet
              (bowerMainErrors, bowerMainPackages) = partitionEithers deps
              (bowerDevErrors, bowerDevPackages) = partitionEithers devDeps
              bowerErrors = bowerMainErrors <> bowerDevErrors

          if null bowerErrors
            then do
              logInfo "All Bower dependencies are in the set! 🎉"
              logInfo $ "You can now safely delete your " <> surroundQuote "bower.json"
            else do
              logWarn $ display $ showBowerErrors bowerErrors

          void $ withConfigAST $ \expr -> do
            let withBowerName = updateName bowerName expr
            addRawDeps config Targets.mainTarget bowerMainPackages withBowerName
              >>= addRawDeps config Targets.testTarget bowerDevPackages

    _ -> pure ()
  -- at last we return the new config
  case eitherConfig of
    Right c -> pure c
    Left err -> die [err]


migrateBower :: Bower.PackageMeta -> PackageSet -> (Text, [Either BowerDependencyError PackageName], [Either BowerDependencyError PackageName])
migrateBower Bower.PackageMeta{..} PackageSet{..} = (packageName, dependencies, devDependencies)
  where
    dependencies = map migratePackage bowerDependencies
    devDependencies = map migratePackage bowerDevDependencies

    -- | For each Bower dependency, we:
    --   * try to parse the range into a SemVer.Range
    --   * then check if it's a purescript package
    --   * then try to search in the Package Set for that package
    --   * then try to match the version there into the Bower range
    migratePackage :: (Bower.PackageName, Bower.VersionRange) -> Either BowerDependencyError PackageName
    migratePackage (Bower.runPackageName -> name, Bower.VersionRange unparsedRange) =
      case SemVer.parseSemVerRange unparsedRange of
        Left _err -> Left $ UnparsableRange (PackageName name) unparsedRange
        Right range -> case Text.stripPrefix "purescript-" name of
          Nothing -> Left $ NonPureScript name
          Just packageSetName | package <- PackageName packageSetName -> case Map.lookup package packagesDB of
            Nothing -> Left $ MissingFromTheSet package
            Just Package{ location = Local _ } -> Right package
            Just Package{ location = Remote {..} } -> case SemVer.parseSemVer version of
              Right v | SemVer.matches range v -> Right package
              _                                -> Left $ WrongVersion package range version

    packageName =
      let name = Bower.runPackageName bowerName
      in case Text.isPrefixOf "purescript-" name of
        True  -> Text.drop 11 name
        False -> name

data BowerDependencyError
  = UnparsableRange PackageName Text
  | NonPureScript Text
  | MissingFromTheSet PackageName
  | WrongVersion PackageName SemVer.SemVerRange Text
  deriving (Eq, Ord)


showBowerErrors :: [BowerDependencyError] -> Text
showBowerErrors (List.sort -> errors)
  = "\n\nSpago encountered some errors while trying to migrate your Bower config.\n"
  <> "A Spago config has been generated but it's recommended that you apply the suggestions here\n\n"
  <> (Text.unlines $ map (\errorGroup ->
      (case List.head errorGroup of
         UnparsableRange _ _ -> "It was not possible to parse the version range for these packages:"
         NonPureScript _ -> "These packages are not PureScript packages, so you should install them with `npm` instead:"
         MissingFromTheSet _ -> "These packages are missing from the package set. You should add them in your local package set:\n(See here for how: https://github.com/purescript/spago#add-a-package-to-the-package-set)"
         WrongVersion _ _ _ -> "These packages are in the set, but did not match the Bower range. You can try to install them with `spago install some-package-name`")
      <> "\n"
      <> Text.unlines (map (("* " <>) . showE) errorGroup)) (List.groupBy groupFn errors))
  where
    groupFn (UnparsableRange _ _) (UnparsableRange _ _) = True
    groupFn (NonPureScript _) (NonPureScript _)         = True
    groupFn (MissingFromTheSet _) (MissingFromTheSet _) = True
    groupFn (WrongVersion _ _ _) (WrongVersion _ _ _)   = True
    groupFn _ _                                         = False

    showE (UnparsableRange (PackageName name) range) = surroundQuote name <> " had range " <> surroundQuote range
    showE (NonPureScript name) = surroundQuote name
    showE (MissingFromTheSet (PackageName name)) = surroundQuote name
    showE (WrongVersion (PackageName name) range version) = surroundQuote name <> " has version " <> version <> ", but range is " <> tshow range


updateName :: Text -> Expr -> Expr
updateName newName (Dhall.RecordLit kvs)
  | Just _name <- Dhall.Map.lookup "name" kvs = Dhall.RecordLit
    $ Dhall.Map.insert "name" (Dhall.makeRecordField $ Dhall.toTextLit newName) kvs
updateName _ other = other

addRawDeps :: HasLogFunc env => Config -> TargetName -> [PackageName] -> Expr -> RIO env Expr
addRawDeps config tgtName newPackages rawExpr =
  case NonEmpty.nonEmpty notInPackageSet of
    Just pkgs -> do
      logWarn $ display $ Messages.failedToAddDeps $ NonEmpty.map packageName pkgs
      pure rawExpr
    Nothing -> case rawExpr of
      Dhall.RecordLit kvs -> Dhall.RecordLit <$> insertDepsIntoTargetsMap kvs
      Dhall.Let b@Dhall.Binding { variable = varName, value = v } inExpr -> do
        (mbBindingName, newInExpr) <- updateOrFindBindingName inExpr
        case mbBindingName of
          Nothing -> pure $ Dhall.Let b newInExpr
          Just bindingName
            | bindingName == varName -> do
                (\newV -> Dhall.Let (Dhall.makeBinding varName newV) inExpr) <$> insertDepsIntoTarget v
            | otherwise -> do
                logWarn $ "Binding for variable '" <> display bindingName <> "' could not be found."
                pure rawExpr
      other -> do
        logWarn $ "Expression was not a record but was " <> display (pretty other)
        pure rawExpr
  where
    Config { packageSet = PackageSet{..} } = config
    notInPackageSet = filter (\p -> Map.notMember p packagesDB) newPackages

    -- | Code from https://stackoverflow.com/questions/45757839
    nubSeq :: Ord a => Seq a -> Seq a
    nubSeq xs = (fmap fst . Seq.filter (uncurry notElem)) (Seq.zip xs seens)
      where
        seens = Seq.scanl (flip Set.insert) Set.empty xs

    -- | Adds the new packages to the first `ListLit` it finds, traversing from
    -- | left to right,  in case there are multiple `ListAppend` values
    -- |
    -- | ```
    -- | [ "old" ] -> [ "old", "new" ]
    -- | [ "old" ] # list1 -> [ "old", "new" ] # list1
    -- | list # [ "old" ] -> list # [ "old", "new" ]
    -- | list1 # list2 # [ "old" ] -> list1 # list2 # [ "old", "new" ]
    -- | ```
    modifyFirstListLitIfExist = \case
      Dhall.ListLit x dependencies -> do
        Just . Dhall.ListLit x <$> addNewDeps dependencies
      Dhall.ListAppend leftList rightList -> do
        mbLeft <- modifyFirstListLitIfExist leftList
        case mbLeft of
          Just left -> do
            pure $ Just $ Dhall.ListAppend left rightList
          Nothing -> do
            mbRight <- modifyFirstListLitIfExist rightList
            case mbRight of
              Just right -> do
                pure $ Just $ Dhall.ListAppend leftList right
              Nothing -> do
                pure Nothing
      _ -> pure Nothing

    -- | Takes the rightmost `ListLit`'s `dependencies` value
    -- | and combines the newPackages with the original dependencies
    addNewDeps dependencies = do
      oldPackages <- traverse (throws . Dhall.fromTextLit) dependencies
      pure
        $ fmap (Dhall.toTextLit . packageName)
        $ Seq.sort $ nubSeq (Seq.fromList newPackages <> fmap PackageName oldPackages)

    -- | This functions traversals down the AST to determine whether the target is a literal value
    -- | in the final `RecordLit`'s "targets" field or whether it will refer to a binding
    -- | declared previously in the AST. Once identified, the traversal up the AST will
    -- | reconstruct the AST with the update. Either the packages will be added in the final
    -- | `RecordLit` when no bindings are used or it will be added at the binding. 
    -- |
    -- | Since this returns `(Maybe Text, expr)`. the `Maybe Text` represents whether
    -- | a binding was used. If it is `Nothing`, then either no binding was used or
    -- | a binding was used but the update has already occurred at a binding "lower"
    -- | in the AST. If it is `Just bindingName`, then a binding was used and hasn't
    -- | yet been updated.
    updateOrFindBindingName = \case
      expr@(Dhall.RecordLit kvs) -> case Dhall.Map.lookup "targets" kvs of
        Nothing -> do
          logWarn "The 'targets' field was not found"
          pure (Nothing, expr)
        Just Dhall.RecordField { recordFieldValue = targetsFieldVal } -> case targetsFieldVal of
          Dhall.RecordLit targets -> case Dhall.Map.lookup (targetName tgtName) targets of
            Nothing -> do
              logWarn $ "The target named '" <> display (targetName tgtName) <> "' was not found"
              pure (Nothing, expr)
            Just Dhall.RecordField { recordFieldValue = tgt } -> case tgt of
              Dhall.RecordLit _ -> do
                -- no binding was used, so do the update in the record
                newTarget <- insertDepsIntoTarget tgt
                let
                  newExpr =
                    Dhall.RecordLit
                      $ flip (Dhall.Map.insert "targets") kvs
                      $ Dhall.makeRecordField
                      $ Dhall.RecordLit
                      $ flip (Dhall.Map.insert (targetName tgtName)) targets
                      $ Dhall.makeRecordField newTarget
                pure (Nothing, newExpr)
              Dhall.Var (Dhall.V bindingName _) ->
                -- binding was used, so return the binding name
                pure (Just bindingName, expr)
              _ ->
                pure (Nothing, expr)
          _ -> do
            pure (Nothing, expr)
      l@(Dhall.Let b@Dhall.Binding { variable = varName, value = v } inExpr) -> do
        (mbBindingName, newInExpr) <- updateOrFindBindingName inExpr
        case mbBindingName of
          Nothing -> pure (Nothing, Dhall.Let b newInExpr)
          Just bindingName
            | bindingName == varName -> do
                newTarget <- insertDepsIntoTarget v
                pure (Nothing, Dhall.Let (Dhall.makeBinding varName newTarget) inExpr)
            | otherwise -> do
                pure (mbBindingName, l)
      other -> pure (Nothing, other)

    -- | Inserts the new dependencies into the 'targets' field of a RecordLit
    insertDepsIntoTargetsMap kvs = case Dhall.Map.lookup "targets" kvs of
      Nothing -> do
        logWarn "Failed to find the 'targets' field in the record."
        pure kvs
      Just Dhall.RecordField { recordFieldValue = targetsFieldVal } -> case targetsFieldVal of
        Dhall.RecordLit targets -> case Dhall.Map.lookup (targetName tgtName) targets of
          Nothing -> do
            logWarn $ "Failed to find the target '" <> display (targetName tgtName) <> "' in the 'targets' field"
            pure kvs
          Just Dhall.RecordField { recordFieldValue = tgt } -> do
            newTarget <- insertDepsIntoTarget tgt
            pure
              $ flip (Dhall.Map.insert "targets") kvs
              $ Dhall.makeRecordField
              $ Dhall.RecordLit
              $ flip (Dhall.Map.insert (targetName tgtName)) targets
              $ Dhall.makeRecordField newTarget
        other -> do
          logWarn $ "The 'targets' field's value was not a record but was " <> display (pretty other)
          pure kvs

    -- | Inserts the new packages into the target's `dependencies` list
    insertDepsIntoTarget tgt = case tgt of
      Dhall.RecordLit tgtValue -> case Dhall.Map.lookup "dependencies" tgtValue of
        Nothing -> do
          logWarn $ "Target '" <> display (targetName tgtName) <> "' does not have a 'dependencies' field."
          pure tgt
        Just Dhall.RecordField { recordFieldValue = depsFieldVal } -> do
          newVal <- case depsFieldVal of
            Dhall.ListLit x dependencies -> do
              newDeps <- addNewDeps dependencies
              pure $ Dhall.ListLit x newDeps
            original@(Dhall.ListAppend leftList rightList) -> do
              mbLeft <- modifyFirstListLitIfExist leftList
              case mbLeft of
                Just newLeft -> do
                  pure $ Dhall.ListAppend newLeft rightList
                Nothing -> do
                  mbRight <- modifyFirstListLitIfExist rightList
                  case mbRight of
                    Just newRight -> do
                      pure $ Dhall.ListAppend leftList newRight
                    Nothing -> do
                      -- if a ListLit was not found (e.g. `one.deps # two.deps`),
                      -- then we add one ourselves (e.g. `one.deps # two.deps # [ "new "]`)
                      newListLit <- Dhall.ListLit Nothing <$> addNewDeps []
                      let
                        newRight = Dhall.ListAppend rightList newListLit
                      pure $ Dhall.ListAppend leftList newRight
            other -> pure other
          pure
            $ Dhall.RecordLit
            $ flip (Dhall.Map.insert "dependencies") tgtValue
            $ Dhall.makeRecordField newVal
      other -> do
        logWarn $ "The target '" <> display (targetName tgtName) <> "' was not a record but was " <> display (pretty other)
        pure other

addSourcePaths :: Expr -> Expr
addSourcePaths = \case
  Dhall.RecordLit kvs
    | isConfigV1 kvs -> do
        let
          mainDeps = fromMaybe (Dhall.makeRecordField $ Dhall.ListLit Nothing [] ) $ Dhall.Map.lookup "dependencies" kvs
          mainSources = Dhall.makeRecordField $ mkSources "src/**/*.purs"
        Dhall.Let (mainTargetBinding mainDeps mainSources)
          $ Dhall.Let testTargetBinding
          $ Dhall.RecordLit
          $ Dhall.Map.delete "dependencies"
          $ Dhall.Map.delete "sources" kvs
    | isConfigV2 kvs -> do
        let
          mainDeps = fromMaybe (Dhall.makeRecordField $ Dhall.ListLit Nothing [] ) $ Dhall.Map.lookup "dependencies" kvs
          mainSources = fromMaybe (Dhall.makeRecordField $ Dhall.ListLit Nothing [] ) $ Dhall.Map.lookup "sources" kvs
        Dhall.Let (mainTargetBinding mainDeps mainSources)
          $ Dhall.Let testTargetBinding
          $ Dhall.RecordLit
          $ Dhall.Map.delete "dependencies"
          $ Dhall.Map.delete "sources" kvs
  expr -> expr
  where
    mkSources txt = Dhall.ListLit Nothing [ Dhall.toTextLit txt ]

    mainTargetBinding deps sources =
      Dhall.makeBinding "main"
        $ Dhall.RecordLit
        $ Dhall.Map.fromList
          [ ("dependencies", deps )
          , ("sources", sources )
          ]
    testTargetBinding =
      Dhall.makeBinding "test"
        $ Dhall.RecordLit
        $ Dhall.Map.fromList
          [ ("dependencies", Dhall.makeRecordField $ Dhall.ListAppend (referToRecordBinding "main" 0 "dependencies") $ Dhall.ListLit Nothing [] )
          , ("sources", Dhall.makeRecordField $ Dhall.ListAppend (referToRecordBinding "main" 0 "sources") (mkSources "test/**/*.purs") )
          ]

    referToRecordBinding varName idx field = Dhall.Field (Dhall.Var (Dhall.V varName idx)) $ Dhall.makeFieldSelection field

isConfigV1, isConfigV2 :: Dhall.Map.Map Text v -> Bool
isConfigV1 (Set.fromList . Dhall.Map.keys -> configKeySet) =
  let configV1Keys = ["name", "dependencies", "packages"]
  in configKeySet == configV1Keys


isConfigV2 (Set.fromList . Dhall.Map.keys -> configKeySet) =
  let configV2Keys = ["name", "dependencies", "packages", "sources"]
      optionalKeys = ["backend", "license", "repository"]
  in Set.difference configKeySet optionalKeys == configV2Keys


filterDependencies :: Expr -> Expr
filterDependencies (Dhall.RecordLit kvs)
  | isConfigV2 kvs, Just deps <- Dhall.Map.lookup "dependencies" (Dhall.extractRecordValues kvs) = deps
filterDependencies expr = expr


-- | Takes a function that manipulates the Dhall AST of the Config, and tries to run it
--   on the current config. If it succeeds, it writes back to file the result returned.
--   Note: it will pass in the parsed AST, not the resolved one (so e.g. imports will
--   still be in the tree). If you need the resolved one, use `ensureConfig`.
withConfigAST
  :: (HasLogFunc env, HasConfigPath env)
  => (Expr -> RIO env Expr) -> RIO env Bool
withConfigAST transform = do
  ConfigPath path <- view (the @ConfigPath)
  rawConfig <- liftIO $ Dhall.readRawExpr path
  case rawConfig of
    Nothing -> die [ display $ Messages.cannotFindConfig path ]
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      -- Write the new expression only if it has actually changed
      let exprHasChanged = Dhall.Core.denote expr /= newExpr
      if exprHasChanged
        then liftIO $ Dhall.writeRawExpr path (header, newExpr)
        else logDebug "Transformed config is the same as the read one, not overwriting it"
      pure exprHasChanged


transformMExpr
  :: MonadIO m
  => (Dhall.Expr s Dhall.Import -> m (Dhall.Expr s Dhall.Import))
  -> Dhall.Expr s Dhall.Import
  -> m (Dhall.Expr s Dhall.Import)
transformMExpr rules =
  transformMOf
    Dhall.subExpressions
    rules
    . Dhall.Core.denote


-- | Try to add the `newPackages` to the "dependencies" list in the Config.
--   It will not add any dependency if any of them is not in the package set.
--   If everything is fine instead, it will add the new deps, sort all the
--   dependencies, and write the Config back to file.
addDependencies
  :: (HasLogFunc env, HasConfigPath env)
  => Config -> TargetName -> [PackageName] 
  -> RIO env ()
addDependencies config tgtName newPackages = do
  configHasChanged <- withConfigAST $ addRawDeps config tgtName newPackages
  unless configHasChanged $
    logWarn "Configuration file was not updated."
