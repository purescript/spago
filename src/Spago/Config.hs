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


-- | Tries to read in a Spago Config
parseConfig
  :: (HasLogFunc env, HasConfigPath env)
  => RIO env Config
parseConfig = do
  -- Here we try to migrate any config that is not in the latest format
  void $ withConfigAST $ pure . addSourcePaths

  ConfigPath path <- view (the @ConfigPath)
  expr <- liftIO $ Dhall.inputExpr $ "./" <> path
  maybeConfig <- parseConfig' expr
  case maybeConfig of
    Just config -> pure config
    Nothing -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwM $ Dhall.ConfigIsNotRecord e
      Left err -> throwM err

parseConfig'
  :: (HasLogFunc env)
  => ResolvedExpr -> RIO env (Maybe Config)
parseConfig' = \case
  Dhall.RecordLit ks' -> do
    let ks = Dhall.extractRecordValues ks'
    let sourcesType  = Dhall.list (Dhall.auto :: Dhall.Decoder SourcePath)
    name              <- Dhall.requireTypedKey ks "name" Dhall.strictText
    dependencies      <- Dhall.requireTypedKey ks "dependencies" dependenciesType
    configSourcePaths <- Dhall.requireTypedKey ks "sources" sourcesType
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
          void $ withConfigAST ( addRawDeps config pscPackages
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
          let (bowerName, packageResults) = migrateBower packageMeta packageSet
              (bowerErrors, bowerPackages) = partitionEithers packageResults

          if null bowerErrors
            then do
              logInfo "All Bower dependencies are in the set! 🎉"
              logInfo $ "You can now safely delete your " <> surroundQuote "bower.json"
            else do
              logWarn $ display $ showBowerErrors bowerErrors

          void $ withConfigAST ( addRawDeps config bowerPackages
                               . updateName bowerName)

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


updateName :: Text -> Expr -> Expr
updateName newName (Dhall.RecordLit kvs)
  | Just _name <- Dhall.Map.lookup "name" kvs = Dhall.RecordLit
    $ Dhall.Map.insert "name" (Dhall.makeRecordField $ Dhall.toTextLit newName) kvs
updateName _ other = other

addRawDeps :: HasLogFunc env => Config -> [PackageName] -> Expr -> RIO env Expr
addRawDeps config newPackages expr =
  case NonEmpty.nonEmpty notInPackageSet of
    Just pkgs -> do
      logWarn $ display $ Messages.failedToAddDeps $ NonEmpty.map packageName pkgs
      pure expr
    -- If none of the newPackages are outside of the set, add them to existing dependencies
    Nothing -> case expr of
      r@(Dhall.RecordLit kvs) ->
        case Dhall.Map.lookup "dependencies" kvs of
          Just Dhall.RecordField { recordFieldValue = Dhall.ListLit _ dependencies } -> do
            oldPackages <- traverse (throws . Dhall.fromTextLit) dependencies
            let newDepsExpr
                  = Dhall.makeRecordField
                  $ Dhall.ListLit Nothing $ fmap (Dhall.toTextLit . packageName)
                  $ Seq.sort $ nubSeq (Seq.fromList newPackages <> fmap PackageName oldPackages)
            pure $ Dhall.RecordLit $ Dhall.Map.insert "dependencies" newDepsExpr kvs
            where
              -- | Code from https://stackoverflow.com/questions/45757839
              nubSeq :: Ord a => Seq a -> Seq a
              nubSeq xs = (fmap fst . Seq.filter (uncurry notElem)) (Seq.zip xs seens)
                where
                  seens = Seq.scanl (flip Set.insert) Set.empty xs
          Just _ -> do
            logWarn "Failed to add dependencies. The `dependencies` field wasn't a List of Strings."
            pure r
          Nothing -> do
            logWarn "Failed to add dependencies. You should have a record with the `dependencies` key for this to work."
            pure r
      _ ->
        pure expr
  where
    Config { packageSet = PackageSet{..} } = config
    notInPackageSet = filter (\p -> Map.notMember p packagesDB) newPackages

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
  => (ResolvedExpr -> Expr -> RIO env Expr) -> RIO env Bool
withRawConfigAST transform = do
  ConfigPath path <- view (the @ConfigPath)
  rawConfig <- liftIO $ Dhall.readRawExpr path
  normalizedExpr <- liftIO $ Dhall.inputExpr $ "./" <> path
  case rawConfig of
    Nothing -> die [ display $ Messages.cannotFindConfig path ]
    Just (header, expr) -> do
      newExpr <- transform normalizedExpr $ Dhall.Core.denote expr
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
  => Config -> [PackageName] 
  -> RIO env ()
addDependencies Config { packageSet = PackageSet{..} } newPackages = do
  configHasChanged <- case NonEmpty.nonEmpty notInPackageSet of
    Just pkgsNotInPackageSet -> do
      logWarn $ display $ Messages.failedToAddDeps $ NonEmpty.map packageName pkgsNotInPackageSet
      pure False
    Nothing -> do
      withRawConfigAST $ \resolvedExpr expr -> do
        newExpr <- AST.modifyRawConfigExpression (AST.AddPackages newPackages) resolvedExpr expr
        -- Verify that returned expression can produce a `Config` value if parsed
        -- before we return it.
        normalizedExpr <- liftIO $ Dhall.inputExpr $ pretty newExpr
        maybeResult <- (Just newExpr <$ parseConfig' normalizedExpr) `catch` (\(_ :: SomeException) -> pure Nothing)
        case maybeResult of
          Just validatedExpr -> do
            pure validatedExpr
          Nothing -> do
            logWarn "Failed to add dependencies."
            logDebug "Raw AST modification did not produce a valid `spago.dhall` file."
            pure expr

  unless configHasChanged $
    logWarn "Configuration file was not updated."
  where
    notInPackageSet = filter (\p -> Map.notMember p packagesDB) newPackages
