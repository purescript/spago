module Spago.Command.Publish (publish, PublishEnv) where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Tuple as Tuple
import Effect.Ref as Ref
import Node.Path as Path
import Registry.Location as Location
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.Operation.Validation as Operation.Validation
import Registry.PackageName as PackageName
import Registry.Solver as Registry.Solver
import Registry.Version as Version
import Spago.BuildInfo as BuildInfo
import Spago.Command.Build as Build
import Spago.Config (Package(..), WithTestGlobs(..), Workspace, WorkspacePackage, PsaConfig)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Git as Git
import Spago.Json as Json
import Spago.Log as Log
import Spago.Purs (Purs)
import Spago.Purs.Graph as Graph

type PublishData =
  { name :: PackageName
  , location :: Maybe Location
  , ref :: String
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

type PublishEnv a =
  { getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
  , getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
  , getCachedIndex :: Effect ManifestIndex
  , workspace :: Workspace
  , logOptions :: LogOptions
  , git :: Git
  , purs :: Purs
  , selected :: WorkspacePackage
  , dependencies :: Map PackageName Package
  , psaConfig :: PsaConfig
  | a
  }

type PublishArgs = {}

publish :: forall a. PublishArgs -> Spago (PublishEnv a) PublishData
publish _args = do
  -- We'll store all the errors here in this ref, then complain at the end
  resultRef <- liftEffect $ Ref.new (Left List.Nil)
  let
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

  env@
    { selected: selected'
    , workspace
    , purs
    , dependencies
    , logOptions
    , getMetadata
    , getCachedIndex
    } <- ask
  let (selected :: WorkspacePackage) = selected' { hasTests = false }
  let name = selected.package.name
  let strName = PackageName.print name
  let compiler = purs.version
  logDebug $ "Publishing package " <> strName

  -- As first thing we run a build to make sure the package compiles at all
  runSpago
    (env { workspace = workspace { selected = Just selected } })
    (Build.run { depsOnly: false, pursArgs: [] })

  -- We then need to check that the dependency graph is accurate. If not, queue the errors
  let globs = getGlobs selected dependencies
  graphCheckErrors <- Graph.runGraphCheck selected globs []
  for_ graphCheckErrors addError

  -- Check if all the packages have ranges, error if not
  let
    { fail: packagesWithNoRanges, success: depsRanges' } = partitionEithers
      $ map
          ( case _ of
              Tuple pkg Nothing -> Left pkg
              Tuple pkg (Just range) -> Right (Tuple pkg range)
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
  cachedIndex <- liftEffect $ getCachedIndex
  let
    dependencyIndex = map (map (unwrap >>> _.dependencies)) $ ManifestIndex.toMap cachedIndex
    maybeBuildPlan = Registry.Solver.solve dependencyIndex depsRanges

  case selected.package.publish of
    Nothing -> addError $ toDoc "Did not find publishing config: add a valid one in package.publish" -- TODO link to docs
    Just publishConfig -> case publishConfig.location of
      Nothing -> addError $ toDoc "Need to specify a publish.location field."
      Just location -> do
        -- Get the metadata file for this package.
        -- It will exist if the package has been published at some point, it will not if the package is new.
        -- We make a new one if that's the case.
        metadata <- runSpago { logOptions } (getMetadata name) >>= case _ of
          Right (Metadata metadata) -> pure metadata
          Left err -> do
            logDebug $ "Got error while reading metadata file: " <> err
            pure
              { location
              , owners: Nothing -- TODO: get that from the config file
              , published: Map.empty
              , unpublished: Map.empty
              }
        let
          manifest =
            { name
            , location
            , description: selected.package.description
            , dependencies: depsRanges
            , version: publishConfig.version
            , license: publishConfig.license
            , owners: Nothing -- TODO specify owners in spago config
            , files: Nothing -- TODO specify files in spago config
            }

        unless (Operation.Validation.locationMatches (Manifest manifest) (Metadata metadata)) $ addError $ toDoc
          [ "The manifest file specifies a location ("
          , Json.stringifyJson Location.codec manifest.location
          , ") that differs from the location in the registry metadata ("
          , Json.stringifyJson Location.codec metadata.location
          , "). If you would like to change the location of your package you should "
          , "submit a transfer operation."
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
              $ (Map.toUnfoldable dependencies :: Array _)
        if Array.length fail > 0 then do
          addError $ toDoc $ "Could not find a suitable build plan, the following packages do not point to registry versions: "
            <> Json.stringifyJson (CA.array PackageName.codec) fail
        else do
          -- All dependencies come from the registry so we can trust the build plan.
          -- We can then try to build with the dependencies from there.
          case maybeBuildPlan of
            Left errs -> addError $ toDoc
              [ toDoc "Could not solve the package dependencies, errors:"
              , indent $ toDoc $ Array.fromFoldable $ map Registry.Solver.printSolverError errs
              ]
            Right (buildPlan :: Map PackageName Version) -> do
              -- Get the current ref or error out if the git tree is dirty
              Git.getRef >>= case _ of
                Left err -> addError err
                Right ref -> do
                  unlessM (Operation.Validation.containsPursFile (Path.concat [ selected.path, "src" ])) $ addError $ toDoc
                    [ "Your package has no .purs files in the src directory. "
                    , "All package sources must be in the `src` directory, with any additional "
                    , "sources indicated by the `files` key in your manifest."
                    ]

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

                  setResult { name, location: Just location, ref, compiler, resolutions: buildPlan }

  result <- liftEffect $ Ref.read resultRef
  case result of
    Left errors -> do
      logError
        $ Log.bold
            ( toDoc
                $ "Your package "
                <> strName
                <> " is not ready for publishing yet, encountered "
                <> show (List.length errors)
                <> " errors:"
            )
        <> Log.break
      die' $ Array.fromFoldable errors
    Right publishingData@{ resolutions } -> do
      -- Once we are sure that no errors will be produced we can try building with the build plan
      -- from the solver (this is because the build might terminate the process, and we shall output the errors first)
      let buildPlanDependencies = map Config.RegistryVersion resolutions
      runSpago
        ( env
            { workspace = workspace { selected = Just selected }
            , dependencies = buildPlanDependencies
            }
        )
        (Build.run { depsOnly: false, pursArgs: [] })

      logDebug $ unsafeStringify publishingData
      logSuccess "Ready for publishing. Calling the registry.."
      pure publishingData

getGlobs :: WorkspacePackage -> Map PackageName Package -> Set FilePath
getGlobs selected dependencies = Set.fromFoldable
  $ join [ Config.sourceGlob NoTestGlobs selected.package.name (WorkspacePackage selected) ]
  <> join (map (Tuple.uncurry $ Config.sourceGlob NoTestGlobs) (Map.toUnfoldable dependencies))
  <> [ BuildInfo.buildInfoPath ]
