module Spago.Command.Publish (publish, PublishEnv) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.List as List
import Data.Map as Map
import Data.String as String
import Dodo (break, lines)
import Effect.Ref as Ref
import Node.Path as Node.Path
import Node.Process as Process
import Record as Record
import Registry.Internal.Path as Internal.Path
import Registry.Location as Location
import Registry.Metadata as Metadata
import Registry.Operation as Operation
import Registry.Operation.Validation as Operation.Validation
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Command.Build (BuildEnv)
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Db (Db)
import Spago.FS as FS
import Spago.Git (Git)
import Spago.Git as Git
import Spago.Json as Json
import Spago.Log as Log
import Spago.Path as Path
import Spago.Prelude as Effect
import Spago.Purs (Purs)
import Spago.Purs.Graph as Graph
import Spago.Registry (PreRegistryEnv)
import Spago.Registry as Registry

type PublishData =
  { name :: PackageName
  , location :: Maybe Location
  , ref :: String
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

type PublishEnv a =
  { getRegistry :: Spago (PreRegistryEnv ()) Registry.RegistryFunctions
  , workspace :: Workspace
  , logOptions :: LogOptions
  , rootPath :: RootPath
  , offline :: OnlineStatus
  , git :: Git
  , db :: Db
  , purs :: Purs
  , selected :: WorkspacePackage
  , dependencies :: Fetch.PackageTransitiveDeps
  | a
  }

type PublishArgs = {}

publish :: ∀ a. PublishArgs -> Spago (PublishEnv a) Operation.PublishData
publish _args = do
  -- We'll store all the errors here in this ref, then complain at the end
  resultRef <- liftEffect $ Ref.new (Left List.Nil)
  let
    setResult :: { expectedVersion :: Version, publishingData :: PublishData } -> _
    setResult r = liftEffect $ Ref.modify_
      ( case _ of
          Left List.Nil -> Right r
          a -> a
      )
      resultRef

    addError :: Docc -> Spago (PublishEnv a) Unit
    addError err = do
      liftEffect $ Ref.modify_
        ( case _ of
            Left errs -> Left (List.Cons err errs)
            Right _r -> Left (List.Cons err List.Nil)
        )
        resultRef

  env@{ selected: selected', purs, dependencies, rootPath } <- ask
  let (selected :: WorkspacePackage) = selected' { hasTests = false }
  let name = selected.package.name
  let strName = PackageName.print name
  let compiler = purs.version
  logDebug $ "Publishing package " <> strName

  -- As first thing we run a build to make sure the package compiles at all
  built <- runBuild { selected, dependencies }
    ( Build.run
        { depsOnly: false
        , pursArgs: []
        , jsonErrors: false
        }
    )
  -- There's a pending failure exit and its' easier to just abort here
  when (not built) $
    Effect.liftEffect Process.exit

  -- We then need to check that the dependency graph is accurate. If not, queue the errors
  let coreDependencies = dependencies # Map.lookup name <#> _.core # fromMaybe Map.empty
  let globs = Build.getBuildGlobs { rootPath, selected: NEA.singleton selected, withTests: false, dependencies: coreDependencies, depsOnly: false }
  eitherGraph <- Graph.runGraph rootPath globs []
  case eitherGraph of
    Right graph -> do
      graphCheckErrors <- Graph.toImportErrors selected { reportSrc: true, reportTest: false }
        <$> runSpago (Record.union { selected, workspacePackages: Config.getWorkspacePackages env.workspace.packageSet } env) (Graph.checkImports graph)
      for_ graphCheckErrors (addError <<< Graph.formatImportErrors <<< pure)
    Left err ->
      die err

  -- Check if all the packages have ranges, error if not
  let
    { fail: packagesWithNoRanges, success: depsRanges' } = partitionEithers
      $ map
          ( case _ of
              Tuple pkg Nothing -> Left pkg
              Tuple pkg (Just constraint) -> Right (Tuple pkg (Config.constraintToRange (Just constraint)))
          )
      $ (Map.toUnfoldable :: Map _ _ -> Array _)
      $ unwrap selected.package.dependencies
    depsRanges = Map.fromFoldable depsRanges'

  unless (Array.null packagesWithNoRanges) do
    addError $ toDoc
      [ toDoc $ "The configuration is missing version bounds for some packages. Run `spago fetch --ensure-ranges` to add them:"
      , indent $ toDoc $ map (\p -> "- " <> PackageName.print p) $ Array.fromFoldable packagesWithNoRanges
      ]

  -- Solve with these ranges to get a build plan
  buildPlan <- Fetch.getTransitiveDepsFromRegistry depsRanges Map.empty

  case selected.package.publish of
    Nothing -> addError $ toDoc
      [ "Did not find publishing config: add a valid one in `package.publish`."
      , "See the configuration file's documentation: https://github.com/purescript/spago#the-configuration-file"
      ]
    Just publishConfig -> case publishConfig.location of
      Nothing -> do
        addedToConfig <- inferLocationAndWriteToConfig selected
        if addedToConfig then
          addError $ toDoc $
            "The `publish.location` field of your `spago.yaml` file was empty. Spago filled it in, please commit it and try again."
        else
          addError $ toDoc
            [ "The `publish.location` field is not present in the `spago.yaml` file."
            , "Spago is unable to infer it from Git remotes, so please populate it manually."
            , "See the docs for more info: https://github.com/purescript/spago#the-configuration-file"
            ]
      Just location -> do
        -- Get the metadata file for this package.
        -- It will exist if the package has been published at some point, it will not if the package is new.
        -- We make a new one if that's the case.
        metadata <- Registry.getMetadata name >>= case _ of
          Right (Metadata metadata) -> pure metadata
          Left err -> do
            logDebug $ "Got error while reading metadata file: " <> err
            pure
              { location
              , owners: NEA.fromArray =<< publishConfig.owners
              , published: Map.empty
              , unpublished: Map.empty
              }
        let
          expectedTag = "v" <> Version.print publishConfig.version
          manifest =
            { name
            , location
            , description: selected.package.description
            , dependencies: depsRanges
            , version: publishConfig.version
            , license: publishConfig.license
            , owners: NEA.fromArray =<< publishConfig.owners
            , excludeFiles: Nothing -- TODO specify files in spago config
            , includeFiles: Nothing -- TODO specify files in spago config
            , ref: expectedTag
            }

        unless (Operation.Validation.locationMatches (Manifest manifest) (Metadata metadata)) $ addError $ toDoc
          [ "The manifest file specifies a location ("
          , Json.stringifyJson Location.codec manifest.location
          , ") that differs from the location in the registry metadata ("
          , Json.stringifyJson Location.codec metadata.location
          , "). If you would like to change the location of your package you should "
          , "submit a transfer operation."
          ]

        locationResult <- locationIsInGitRemotes rootPath location
        unless locationResult.result $ addError $ toDoc
          [ toDoc "The location specified in the manifest file is not one of the remotes in the git repository."
          , toDoc "Location:"
          , indent (toDoc $ "- " <> prettyPrintLocation location)
          , toDoc "Remotes:"
          , lines $ locationResult.remotes <#> \r -> indent $ toDoc $ "- " <> r.name <> ": " <> r.url
          ]

        -- Check that all the dependencies come from the registry
        let
          { fail, success: _ } =
            partitionEithers
              $ map
                  ( \(Tuple pkgName pkg) -> case pkg of
                      RegistryVersion v -> Right (Tuple pkgName v)
                      _ -> Left pkgName
                  )
              $ (Map.toUnfoldable coreDependencies :: Array _)
        if Array.length fail > 0 then
          addError
            $ toDoc
                [ "Some of the packages you specified as `extraPackages` do not point to the Registry."
                , "To be able to publish a package to the registry, all of its dependencies have to be packages registered on the Registry."
                , "Please replace the following packages with versions that are present in the Registry:" -- TODO point to docs
                ]
            <> break
            <> lines (fail <#> \p -> indent $ toDoc $ "- " <> PackageName.print p)
        else do
          -- All dependencies come from the registry so we can trust the build plan.
          -- We can then try to build with the dependencies from there.

          Internal.Path.readPursFiles (Path.toRaw $ selected.path </> "src") >>= case _ of
            Nothing -> addError $ toDoc
              [ "This package has no PureScript files in its `src` directory. "
              , "All package sources must be in the `src` directory, with any additional "
              , "sources indicated by the `files` key in your manifest."
              ]
            Just files -> do
              let
                -- `validatePursModules` returns full paths in its response, so
                -- we need to strip the workspace root path to print it out in
                -- user-friendly way, but we can't use the machinery from
                -- `Spago.Paths`, because the paths are embedded in other text,
                -- so we have to resort to substring matching.
                rootPathPrefix =
                  Path.toRaw rootPath
                    # String.stripSuffix (String.Pattern "/")
                    # fromMaybe (Path.toRaw rootPath)
                    # (_ <> Node.Path.sep)
              Operation.Validation.validatePursModules files >>= case _ of
                Left formattedError -> addError $ toDoc
                  [ "This package has either malformed or disallowed PureScript module names"
                  , "in its `src` directory. All package sources must be in the `src` directory,"
                  , "with any additional sources indicated by the `files` key in your manifest."
                  , formattedError # String.replaceAll (String.Pattern rootPathPrefix) (String.Replacement "")
                  ]
                Right _ -> pure unit

          when (Operation.Validation.isMetadataPackage (Manifest manifest)) do
            addError $ toDoc "The `metadata` package cannot be uploaded to the registry because it is a protected package."

          for_ (Operation.Validation.isNotPublished (Manifest manifest) (Metadata metadata)) \info -> addError $ toDoc
            [ "You tried to upload a version that already exists: " <> Version.print manifest.version
            , "Its metadata is:"
            , "```json"
            , printJson Metadata.publishedMetadataCodec info
            , "```"
            ]
          for_ (Operation.Validation.isNotUnpublished (Manifest manifest) (Metadata metadata)) \info -> addError $ toDoc
            [ "You tried to upload a version that has been unpublished: " <> Version.print manifest.version
            , ""
            , "```json"
            , printJson Metadata.unpublishedMetadataCodec info
            , "```"
            ]

          -- These are "soft" git tag checks. We notify the user of errors
          -- they need to fix. But these commands must not have the user
          -- 1) create/push a git tag that is known to be unpublishable,
          --    thereby forcing them to create another git tag later with the fix.
          -- 2) input any login credentials as there are other errors to fix
          --    before doing that.
          -- The "hard" git tag checks will occur only if these succeed.
          Git.getStatus rootPath >>= case _ of
            Left _err -> do
              die $ toDoc
                [ toDoc "Could not verify whether the git tree is clean. Error was:"
                , indent _err
                ]
            Right statusResult
              | statusResult /= "" ->
                  addError $ toDoc
                    [ toDoc "The git tree is not clean. Please commit or stash these files:"
                    , indent $ toDoc (String.split (String.Pattern "\n") statusResult)
                    ]
              | otherwise -> do
                  -- TODO: once we ditch `purs publish`, we don't have a requirement for a tag anymore,
                  -- but we can use any ref. We can then use `getRef` here instead of `tagCheckedOut`
                  maybeCurrentTag <- hush <$> Git.tagCheckedOut rootPath
                  case maybeCurrentTag of
                    Just currentTag ->
                      when (currentTag /= expectedTag) $ addError $ toDoc
                        [ "The tag (" <> currentTag <> ") does not match the expected tag (" <> expectedTag <> ")."
                        , "Fix all other publishing-related errors first before creating the correct tag. Do not push your created tag to its remote. Prematurely creating and pushing a tag can lead to unpublishable tags."
                        , ""
                        , "To create the tag, you can run:"
                        , ""
                        , "  git tag " <> expectedTag
                        ]
                    Nothing ->
                      -- Current commit does not refer to a git tag.
                      -- We should see whether the expected tag was already defined
                      Git.listTags rootPath >>= case _ of
                        Left err ->
                          die $ toDoc
                            [ toDoc "Cannot check whether publish config's `version` matches any existing git tags due to the below error:"
                            , err
                            ]
                        Right tags -> do
                          let
                            tagExists = Array.any (eq expectedTag) tags
                            emptyUnless b xs = if b then [] else xs
                          when tagExists $ addError $ toDoc
                            [ "The expected tag (" <> expectedTag <> ") has already been defined but is not checked out."
                            , "The publish config's `version` may need to be bumped to a newer version if it currently refers to a version that has already been published."
                            ]
                          addError $ toDoc
                            $
                              [ "No git tag is currently checked out."
                              , "Please fix all other publishing-related errors before resolving this one. Prematurely creating and pushing a tag may lead to unpublishable tags."
                              ]
                            <> emptyUnless tagExists
                              [ "To create the tag, you can run:"
                              , ""
                              , "  git tag " <> expectedTag
                              ]

          setResult
            { expectedVersion: publishConfig.version
            , publishingData: { name, location: Just location, ref: expectedTag, compiler, resolutions: buildPlan }
            }

  result <- liftEffect $ Ref.read resultRef
  case result of
    Left errors -> do
      logError
        $ Log.bold
            ( toDoc
                $ "Your package "
                <> show strName
                <> " is not ready for publishing yet, encountered "
                <> show (List.length errors)
                <> if (List.length errors == 1) then " error:" else " errors:"
            )
        <> Log.break
      die' $ Array.fromFoldable errors
    Right { expectedVersion, publishingData: publishingData@{ resolutions } } -> do
      logInfo "Passed preliminary checks."
      -- This requires login credentials.
      Git.pushTag rootPath expectedVersion >>= case _ of
        Left err -> die $ toDoc
          [ err
          , toDoc "You can try to push the tag manually by running:"
          , toDoc ""
          , toDoc $ "  git push origin v" <> Version.print expectedVersion
          ]
        Right _ -> pure unit

      -- Once we are sure that no errors will be produced we can try building with the build plan
      -- from the solver (this is because the build might terminate the process, and we shall output the errors first)
      logInfo "Building again with the build plan from the solver..."
      let buildPlanDependencies = map Config.RegistryVersion resolutions
      Fetch.fetchPackagesToLocalCache buildPlanDependencies
      builtAgain <- runBuild { selected, dependencies: Map.singleton selected.package.name { core: buildPlanDependencies, test: Map.empty } }
        ( Build.run
            { depsOnly: false
            , pursArgs: []
            , jsonErrors: false
            }
        )

      -- As above: there's a pending failure exit and it's easier to just abort here
      when (not builtAgain) $
        Effect.liftEffect Process.exit

      logDebug $ unsafeStringify publishingData
      logSuccess "Ready for publishing. Calling the registry.."

      let
        newPublishingData :: Operation.PublishData
        newPublishingData =
          { name: publishingData.name
          , location: publishingData.location
          , ref: publishingData.ref
          , version: expectedVersion
          , compiler: Just publishingData.compiler
          , resolutions: Just publishingData.resolutions
          }
      Registry.submitRegistryOperation (Operation.Publish newPublishingData)
      pure newPublishingData
  where
  -- If you are reading this and think that you can make it look nicer with
  -- a `Record.merge` then please, be my guest
  runBuild :: _ -> Spago (BuildEnv _) _ -> Spago (PublishEnv _) _
  runBuild { selected, dependencies } action = do
    env <- ask
    runSpago
      { purs: env.purs
      , git: env.git
      , dependencies
      , logOptions: env.logOptions
      , rootPath: env.rootPath
      , workspace: env.workspace { selected = Just selected }
      , strictWarnings: Nothing
      , pedanticPackages: false
      }
      action

locationIsInGitRemotes :: ∀ a. RootPath -> Location -> Spago (PublishEnv a) { result :: Boolean, remotes :: Array Git.Remote }
locationIsInGitRemotes root location = do
  isGitRepo <- FS.exists $ root </> ".git"
  if not isGitRepo then
    pure { result: false, remotes: [] }
  else
    Git.getRemotes root >>= case _ of
      Left err ->
        die [ toDoc "Couldn't parse Git remotes: ", err ]
      Right remotes -> do
        let
          result = remotes # Array.any \r -> case location of
            Location.Git { url } -> r.url == url
            Location.GitHub { owner, repo } -> r.owner == owner && r.repo == repo
        pure { result, remotes }

inferLocationAndWriteToConfig :: ∀ a. WorkspacePackage -> Spago (PublishEnv a) Boolean
inferLocationAndWriteToConfig selectedPackage = do
  { rootPath } <- ask
  Git.getRemotes rootPath >>= case _ of
    Left err ->
      die [ toDoc "Couldn't parse Git remotes: ", err ]
    Right remotes ->
      case Array.find (_.name >>> eq "origin") remotes of
        Nothing ->
          pure false
        Just origin -> do
          -- TODO: at the moment the registry supports only `subdir:
          -- Nothing` cases, so we error out when the package is nested.
          -- Once the registry supports subdirs, the `else` branch should
          -- return `selectedPackage.path`
          subdir <-
            if Config.isRootPackage selectedPackage then
              pure Nothing
            else
              die "The registry does not support nested packages yet. Only the root package can be published."

          -- TODO: similarly, the registry only supports `GitHub` packages, so
          -- we error out in other cases. Once the registry supports non-GitHub
          -- packages, the `else` branch should return `Git { url: origin.url, subdir }`
          location <-
            if String.contains (String.Pattern "github.com") origin.url then
              pure $ Location.GitHub { owner: origin.owner, repo: origin.repo, subdir }
            else
              die
                [ "The registry only supports packages hosted on GitHub at the moment."
                , "Cannot publish this package because it is hosted at " <> origin.url
                ]

          doc <- justOrDieWith selectedPackage.doc Config.configDocMissingErrorMessage
          liftEffect $ Config.addPublishLocationToConfig doc location
          liftAff $ FS.writeYamlDocFile (selectedPackage.path </> "spago.yaml") doc
          pure true

prettyPrintLocation :: Location -> String
prettyPrintLocation = case _ of
  Location.Git { url } -> url
  Location.GitHub { owner, repo } -> "GitHub: " <> owner <> "/" <> repo
