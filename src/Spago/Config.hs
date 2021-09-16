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
import qualified Spago.Config.AST      as AST
import qualified Spago.PackageSet      as PackageSet
import qualified Spago.PscPackage      as PscPackage
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


-- | Tries to parse the raw Dhall expression stored
-- in the @./spago.dhall@ file into a `Config` value.
parseConfig
  :: (HasLogFunc env, HasConfigPath env)
  => RIO env Config
parseConfig = do
  -- Here we try to migrate any config that is not in the latest format
  void $ withRawConfigAST $ AST.modifyRawConfigExpression AST.MigrateFromV1

  ConfigPath path <- view (the @ConfigPath)
  expr <- liftIO $ Dhall.inputExpr $ "./" <> path
  maybeConfig <- parseConfigNormalizedExpr expr
  case maybeConfig of
    Just config -> pure config
    Nothing -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwM $ Dhall.ConfigIsNotRecord e
      Left err -> throwM err

-- |
-- Attempts to parse a normalized Dhall expression (i.e. all imports have been resolved)
-- into a `Config` value.
parseConfigNormalizedExpr
  :: (HasLogFunc env)
  => ResolvedExpr -> RIO env (Maybe Config)
parseConfigNormalizedExpr = \case
  Dhall.RecordLit ks' -> do
    let ks = Dhall.extractRecordValues ks'
    let sourcesType  = Dhall.list (Dhall.auto :: Dhall.Decoder SourcePath)
    name              <- Dhall.requireTypedKey ks "name" Dhall.strictText
    dependencies      <- List.sort <$> Dhall.requireTypedKey ks "dependencies" dependenciesType
    configSourcePaths <- List.sort <$> Dhall.requireTypedKey ks "sources" sourcesType
    alternateBackend  <- Dhall.maybeTypedKey ks "backend" Dhall.strictText

    let ensurePublishConfig = do
          publishLicense    <- Dhall.requireTypedKey ks "license" Dhall.strictText
          publishRepository <- Dhall.requireTypedKey ks "repository" Dhall.strictText
          pure PublishConfig{..}
    publishConfig <- try ensurePublishConfig

    packageSet <- Dhall.requireKey ks "packages" (\case
      Dhall.RecordLit pkgs -> parsePackageSet (Dhall.extractRecordValues pkgs)
      something            -> throwM $ Dhall.PackagesIsNotRecord something)

    pure $ Just Config{..}
  _ ->
    pure Nothing

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
makeTempConfig dependencies alternateBackend configSourcePaths maybeTag = do
  PursCmd { compilerVersion } <- view (the @PursCmd)
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
          updateName config (PscPackage.name pscConfig)
          addDependencies config pscPackages
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
          let (bowerName, packageResults) = migrateBower packageMeta packageSet
              (bowerErrors, bowerPackages) = partitionEithers packageResults

          if null bowerErrors
            then do
              logInfo "All Bower dependencies are in the set! 🎉"
              logInfo $ "You can now safely delete your " <> surroundQuote "bower.json"
            else do
              logWarn $ display $ showBowerErrors bowerErrors

          updateName config bowerName
          addDependencies config bowerPackages

    _ -> pure ()
  -- at last we return the new config
  case eitherConfig of
    Right c -> pure c
    Left err -> die [err]


migrateBower :: Bower.PackageMeta -> PackageSet -> (Text, [Either BowerDependencyError PackageName])
migrateBower Bower.PackageMeta{..} PackageSet{..} = (packageName, dependencies)
  where
    dependencies = map migratePackage (bowerDependencies <> bowerDevDependencies)

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

addSourcePaths :: Expr -> Expr
addSourcePaths (Dhall.RecordLit kvs)
  | isConfigV1 kvs =
    let sources = Dhall.ListLit Nothing $ fmap Dhall.toTextLit $ Seq.fromList ["src/**/*.purs", "test/**/*.purs"]
    in Dhall.RecordLit (Dhall.Map.insert "sources" (Dhall.makeRecordField sources) kvs)
addSourcePaths expr = expr

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

-- | Takes a function that manipulates the Dhall AST of the Config, and tries to run it
--   on the current config. If it succeeds, it writes back to file the result returned.
withRawConfigAST
  :: (HasLogFunc env, HasConfigPath env)
  => (AST.ResolvedUnresolvedExpr -> RIO env Expr) -> RIO env Bool
withRawConfigAST transform = do
  ConfigPath path <- view (the @ConfigPath)
  rawConfig <- liftIO $ Dhall.readRawExpr path
  normalizedExpr <- liftIO $ Dhall.inputExpr $ "./" <> path
  case rawConfig of
    Nothing -> die [ display $ Messages.cannotFindConfig path ]
    Just (header, expr) -> do
      let
        unresolved = Dhall.Core.denote expr
        resolved = normalizedExpr

      newExpr <- transform $ AST.ResolvedUnresolvedExpr (resolved, unresolved)
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

updateName
  :: forall env
   . (HasLogFunc env, HasConfigPath env)
  => Config
  -> Text
  -> RIO env ()
updateName config newName = do
  configHasChanged <- withRawConfigAST $ \sameExpr -> do
    let
      expectedConfig :: Config
      expectedConfig = config { name = newName }
    newExpr <- AST.modifyRawConfigExpression (AST.UpdateName newName) sameExpr
    -- Verify that returned expression produces the expected `Config` value if parsed
    -- before we return it.
    normalizedExpr <- liftIO $ Dhall.inputExpr $ pretty newExpr
    maybeResult <- parseConfigNormalizedExpr normalizedExpr `catch` (\(_ :: SomeException) -> pure Nothing)
    case maybeResult of
      Just parsedConfig -> do
        validModification <- expectedConfig `isSemanticallyEquivalentTo` parsedConfig
        if validModification then do
          pure newExpr
        else do
          logWarn "Failed to update name."
          logDebug $
            "Raw AST modification did not produce the expected Dhall expression. " <>
            "If parsed in a future command, the AST would not produce the expected `Config` value."
          pure $ snd $ AST.resolvedUnresolvedExpr sameExpr
      Nothing -> do
        logWarn "Failed to update name."
        logDebug "Raw AST modification did not produce a valid `spago.dhall` file."
        pure $ snd $ AST.resolvedUnresolvedExpr sameExpr
  unless configHasChanged $
    logWarn "Configuration file was not updated."

-- | Try to add the `newPackages` to the "dependencies" list in the Config.
--   It will not add any dependency if any of them is not in the package set.
--   If everything is fine instead, it will add the new deps, sort all the
--   dependencies, and write the Config back to file.
addDependencies
  :: forall env
   . (HasLogFunc env, HasConfigPath env)
  => Config -> [PackageName] 
  -> RIO env ()
addDependencies config@Config { dependencies = deps, publishConfig = pubConfig } newPackages = do
  configHasChanged <- case notInPackageSet config newPackages of
    Just pkgsNotInPackageSet -> do
      logWarn $ display $ Messages.failedToAddDeps $ NonEmpty.map packageName pkgsNotInPackageSet
      pure False
    Nothing -> do
      let
        expectedConfig :: Config
        expectedConfig = config { dependencies = mkExpectedConfigDeps, publishConfig = mkExpectedPubConifg }
      withRawConfigAST $ \sameExpr -> do
        newExpr <- AST.modifyRawConfigExpression (AST.AddPackages newPackages) sameExpr
        -- Verify that returned expression produces the expected `Config` value if parsed
        -- before we return it.
        normalizedExpr <- liftIO $ Dhall.inputExpr $ pretty newExpr
        maybeResult <- parseConfigNormalizedExpr normalizedExpr `catch` (\(_ :: SomeException) -> pure Nothing)
        case maybeResult of
          Just parsedConfig -> do
            validModification <- expectedConfig `isSemanticallyEquivalentTo` parsedConfig
            if validModification then do
              pure newExpr
            else do
              logWarn "Failed to add dependencies."
              logDebug $
                "Raw AST modification did not produce the expected Dhall expression. " <>
                "If parsed in a future command, the AST would not produce the expected `Config` value."
              pure $ snd $ AST.resolvedUnresolvedExpr sameExpr
          Nothing -> do
            logWarn "Failed to add dependencies."
            logDebug "Raw AST modification did not produce a valid `spago.dhall` file."
            pure $ snd $ AST.resolvedUnresolvedExpr sameExpr

  unless configHasChanged $
    logWarn "Configuration file was not updated."

  where
    mkExpectedConfigDeps = List.nub $ List.sort $ deps <> newPackages

    -- |
    -- If the @pubConfig@ parsing fails, it will fail on the first key checked (i.e. the @license@ key).
    -- When it does fail, it records a map of the expression and that map does not include the new packages
    -- When the modified expression is parsed, it will also fail at the @license@ key. However, it\'s
    -- map will include the new packages.
    --
    -- Thus, we need to update the map in the expected config, so the equality check will pass.
    mkExpectedPubConifg = case pubConfig of
      Left (Dhall.RequiredKeyMissing key kvs) ->
        Left (Dhall.RequiredKeyMissing key newKvs)
        where
          newKvs = Dhall.Map.insertWith insertNewPackages "dependencies" newPackagesExpr kvs

          newPackagesExpr :: ResolvedExpr
          newPackagesExpr = Dhall.ListLit Nothing $ Seq.fromList $ fmap (Dhall.toTextLit . packageName) newPackages

          insertNewPackages :: ResolvedExpr -> ResolvedExpr -> ResolvedExpr
          insertNewPackages (Dhall.ListLit an left) (Dhall.ListLit _ right) =
            Dhall.ListLit an $ nubSeq $ left <> right
          insertNewPackages other _ = other

      x -> x

-- |
-- Unfortunately, we cannot just check whether the expected config is equal to the actual config
-- because "Dhall.Map.Map" keeps track of order when equating two maps.
-- For some cases, this \"values are only equal if ordered the same\" check will cause a failure when
-- we attempt to parse the @PublishConfig@ and fail. In such circumstances, the failure
-- message will be @Left (Dhall.RequiredKeyMissing licenseOrRepositoryText map)@ and @map@ will have a different
-- order in the expected config than it will in the parsed config.
--
-- Moreover, if the config equality check below fails, it is more helpful to understand what parts of
-- the @Config@ values were considered unequal. Thus, besides doing a typical @expected == actual@ check,
-- we will log debug messages to the console while checking all values in case there are multiple
-- values that are different.
isSemanticallyEquivalentTo
  :: forall env
   . (HasLogFunc env)
  => Config -> Config -> RIO env Bool
isSemanticallyEquivalentTo
  Config { name = expN, dependencies = expD, packageSet = expPS, alternateBackend = expAB, configSourcePaths = expCSP, publishConfig = expPC }
  Config { name = actN, dependencies = actD, packageSet = actPS, alternateBackend = actAB, configSourcePaths = actCSP, publishConfig = actPC }
  = checkAll
      [ checkValue expN actN "Config: name"
      , checkValue expD actD "Config: dependencies"
      , checkValue expPS actPS "Config: package set"
      , checkValue expAB actAB "Config: alternate backend"
      , checkValue expCSP actCSP "Config: config source paths"
      , checkPC expPC actPC
      ]
  where
    checkAll :: [RIO env Bool] -> RIO env Bool
    checkAll = foldl' (\acc n -> do
      prev <- acc
      next <- n
      pure $ prev && next) (pure True)

    checkValue :: forall a. Eq a => Show a => a -> a -> Utf8Builder -> RIO env Bool
    checkValue expected actual msg
      | expected == actual = do
          logDebug $ msg <> " - No problem here."
          pure True
      | otherwise = do
          logDebug $ msg <> " - Found mismatch"
          logDebug $ displayShow expected
          logDebug $ displayShow actual
          pure False

    checkPC :: Either (Dhall.ReadError Void) PublishConfig -> Either (Dhall.ReadError Void) PublishConfig -> RIO env Bool
    checkPC (Right l) (Right r) = do
      checkValue l r "Config: pubConfig - Right"
    checkPC (Left (Dhall.RequiredKeyMissing k1 kvs1)) (Left (Dhall.RequiredKeyMissing k2 kvs2)) = do
      checkAll
        [ checkValue k1 k2 "Config: pubConfig - Left RequiredKeyMissing: keys"
        , checkValue (sortDependencies kvs1) (sortDependencies kvs2) "Config: pubConfig - Left RequiredKeyMissing: maps"
        ]
    checkPC l r = do
      logDebug "Config: pubConfig: unexpected value in both"
      logDebug $ "Expected value: " <> displayShow l
      logDebug $ "Actual value: " <> displayShow r
      pure False

    sortDependencies :: Dhall.Map.Map Text ResolvedExpr -> Dhall.Map.Map Text ResolvedExpr
    sortDependencies x = case Dhall.Map.lookup dependenciesText x of
      Just (Dhall.ListLit a pkgs) ->
        Dhall.Map.insert dependenciesText (Dhall.ListLit a (Seq.sortOn toText pkgs)) x
      _ ->
        x
      where
        dependenciesText = "dependencies"

        toText = \case
          Dhall.TextLit (Dhall.Chunks [] t) -> t
          _ -> error "impossible: A normalized expression that produced a valid `Config` value should only have a `TextLit` here"

-- |
-- Returns a non-empty list of packages not found in the package set
-- or @Nothing@ if all are found in the package set.
notInPackageSet
  :: Config -> [PackageName] -> Maybe (NonEmpty PackageName)
notInPackageSet Config { packageSet = PackageSet{..} } newPackages =
   NonEmpty.nonEmpty $ filter (\p -> Map.notMember p packagesDB) newPackages
