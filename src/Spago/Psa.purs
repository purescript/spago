-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa where

import Spago.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Alternative as Alternative
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON as CJ
import Data.Map as Map
import Data.Set as Set
import Data.String as Str
import Data.String as String
import Data.Tuple as Tuple
import Effect.Ref as Ref
import Foreign.Object as FO
import JSON as JSON
import Node.Encoding as Encoding
import Node.FS.Aff as FSA
import Node.Path as Path
import Spago.Cmd as Cmd
import Spago.Config (Package(..), PackageMap, WorkspacePackage)
import Spago.Config as Config
import Spago.Core.Config (CensorBuildWarnings(..), WarningCensorTest(..))
import Spago.Core.Config as Core
import Spago.Log (prepareToDie)
import Spago.Psa.Output (buildOutput)
import Spago.Psa.Printer (printDefaultOutputToErr, printJsonOutputToOut)
import Spago.Psa.Types (ErrorCode, PathDecision, PsaArgs, PsaOutputOptions, PsaPathType(..), psaResultCodec)
import Spago.Purs as Purs

defaultStatVerbosity :: Core.StatVerbosity
defaultStatVerbosity = Core.CompactStats

psaCompile :: forall a. Set.Set FilePath -> Array String -> PsaArgs -> Spago (Purs.PursEnv a) Boolean
psaCompile globs pursArgs psaArgs = do
  result <- Purs.compile globs (Array.snoc pursArgs "--json-errors")
  let resultStdout = Cmd.getStdout result
  arrErrorsIsEmpty <- forWithIndex (Str.split (Str.Pattern "\n") resultStdout) \idx err ->
    case JSON.parse err >>= CJ.decode psaResultCodec >>> lmap CJ.DecodeError.print of
      Left decodeErrMsg -> do
        logWarn $ Array.intercalate "\n"
          [ "Failed to decode PsaResult at index '" <> show idx <> "': " <> decodeErrMsg
          , "Json was: " <> err
          ]
        -- If we can't decode the error, then there's likely a codec issue on Spago's side.
        -- So, this shouldn't fail the build.
        pure true
      Right out -> do
        files <- liftEffect $ Ref.new FO.empty
        out' <- buildOutput (loadLines files) psaArgs out

        liftEffect $ if psaArgs.jsonErrors then printJsonOutputToOut out' else printDefaultOutputToErr psaArgs out'

        pure $ FO.isEmpty out'.stats.allErrors

  if Array.all identity arrErrorsIsEmpty && Cmd.exitedOk result then do
    logSuccess "Build succeeded."
    pure true
  else if not $ Cmd.exitedOk result then do
    prepareToDie [ "purs exited with non-ok status code: " <> show (Cmd.exit result) ]
    pure false
  else do
    case result of
      Left r -> logDebug $ Cmd.printExecResult r
      _ -> pure unit
    prepareToDie [ "Failed to build." ]
    pure false

  where
  isEmptySpan filename pos =
    filename == "" || pos.startLine == 0 && pos.endLine == 0 && pos.startColumn == 0 && pos.endColumn == 0

  -- TODO: Handle exceptions
  loadLines files filename pos
    | isEmptySpan filename pos = pure Nothing
    | otherwise = do
        result <- try do
          cache <- liftEffect $ FO.lookup filename <$> Ref.read files
          contents <-
            case cache of
              Just lines -> pure lines
              Nothing -> do
                lines <- liftAff $ Str.split (Str.Pattern "\n") <$> FSA.readTextFile Encoding.UTF8 filename
                liftEffect $ Ref.modify_ (FO.insert filename lines) files
                pure lines
          let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
          pure $ Just source
        either (const (pure Nothing)) pure result

toPathDecisions
  :: { allDependencies :: PackageMap
     , selectedPackages :: Array WorkspacePackage
     , psaCliFlags :: PsaOutputOptions
     , censorLibWarnings :: Maybe Core.CensorBuildWarnings
     }
  -> Array (Effect (Array (String -> Maybe PathDecision)))
toPathDecisions { allDependencies, selectedPackages, psaCliFlags, censorLibWarnings } =
  projectDecisions <> dependencyDecisions
  where
  projectDecisions = selectedPackages <#> \selected -> toWorkspacePackagePathDecision { selected, psaCliFlags }

  dependencyDecisions =
    map toDependencyDecision
      $ Map.toUnfoldable
      -- Remove workspace packages that are dependencies of some other workspace package
      -- so that we don't add their entries twice
      $ Map.filterKeys (\pkgName -> not $ Set.member pkgName pkgsInProject) allDependencies

  pkgsInProject :: Set PackageName
  pkgsInProject = foldMap (\p -> Set.singleton p.package.name) selectedPackages

  toDependencyDecision :: Tuple PackageName Package -> Effect (Array (String -> Maybe PathDecision))
  toDependencyDecision dep = case snd dep of
    WorkspacePackage p ->
      toWorkspacePackagePathDecision
        { selected: p
        , psaCliFlags
        }
    _ -> do
      pkgLocation <- Path.resolve [] $ Tuple.uncurry Config.getPackageLocation dep
      pure
        [ toPathDecision
            { pathIsFromPackage: isJust <<< String.stripPrefix (String.Pattern pkgLocation)
            , pathType: IsLib
            , strict: false
            , censorWarnings: censorLibWarnings
            }
        ]

toWorkspacePackagePathDecision
  :: { selected :: WorkspacePackage
     , psaCliFlags :: PsaOutputOptions
     }
  -> Effect (Array (String -> Maybe PathDecision))
toWorkspacePackagePathDecision { selected: { path, package }, psaCliFlags } = do
  pkgPath <- Path.resolve [] path
  let srcPath = Path.concat [ pkgPath, "src" ]
  let testPath = Path.concat [ pkgPath, "test" ]
  pure
    [ toPathDecision
        { pathIsFromPackage: isJust <<< String.stripPrefix (String.Pattern srcPath)
        , pathType: IsSrc
        , strict: fromMaybe false $ psaCliFlags.strict <|> (package.build >>= _.strict)
        , censorWarnings: package.build >>= _.censorProjectWarnings
        }
    , toPathDecision
        { pathIsFromPackage: isJust <<< String.stripPrefix (String.Pattern testPath)
        , pathType: IsSrc
        , strict: fromMaybe false $ psaCliFlags.strict <|> (package.test >>= _.strict)
        , censorWarnings: package.test >>= _.censorTestWarnings
        }
    ]

toPathDecision
  :: { pathIsFromPackage :: String -> Boolean
     , pathType :: PsaPathType
     , strict :: Boolean
     , censorWarnings :: Maybe Config.CensorBuildWarnings
     }
  -> String
  -> Maybe PathDecision
toPathDecision options pathToFile = do
  Alternative.guard $ options.pathIsFromPackage pathToFile
  pure
    { pathType: options.pathType
    , shouldPromoteWarningToError: options.strict
    , shouldShowError: shouldPrintWarning options.censorWarnings
    }

shouldPrintWarning :: Maybe Config.CensorBuildWarnings -> ErrorCode -> String -> Boolean
shouldPrintWarning = case _ of
  Nothing -> \_ _ -> true
  Just x -> case x of
    CensorAllWarnings -> \_ _ -> false
    CensorSpecificWarnings arr -> do
      let
        tests = arr <#> case _ of
          ByCode c -> \code _ -> c == code
          ByMessagePrefix prefix -> \_ msg -> isJust $ String.stripPrefix (String.Pattern $ String.trim prefix) (String.trim msg)
      -- We return `true` to print the warning.
      -- If an element was found (i.e. `Just` is returned), then one of the tests succeeded, 
      -- so we should not print the warning and return false here.
      \code msg -> isNothing $ NonEmptyArray.find (\f -> f code msg) tests
