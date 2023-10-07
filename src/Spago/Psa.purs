-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa where

import Spago.Prelude

import Control.Alternative as Alternative
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as Str
import Data.String as String
import Data.Tuple as Tuple
import Effect.Ref as Ref
import Foreign.Object as FO
import Node.Encoding as Encoding
import Node.FS.Aff as FSA
import Node.Path as Path
import Spago.Config (CensorBuildWarnings(..), Package(..), PackageMap, WorkspacePackage)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.Psa.Output (buildOutput)
import Spago.Psa.Printer (printDefaultOutputToErr, printJsonOutputToOut)
import Spago.Psa.Types (ErrorCode, PathDecision, PsaArgs, PsaOutputOptions, PsaPathType(..), WorkspacePsaOutputOptions, psaResultCodec)
import Spago.Purs as Purs

defaultStatVerbosity :: Core.StatVerbosity
defaultStatVerbosity = Core.CompactStats

psaCompile :: forall a. Set.Set FilePath -> Array String -> PsaArgs -> Spago (Purs.PursEnv a) Unit
psaCompile globs pursArgs psaArgs = do
  result <- Purs.compile globs (Array.snoc pursArgs "--json-errors")
  let
    result' = case result of
      Left err -> { output: err.stdout, exitCode: err.exitCode, err: Just err }
      Right success -> { output: success.stdout, exitCode: Just success.exitCode, err: Nothing }
  arrErrorsIsEmpty <- forWithIndex (Str.split (Str.Pattern "\n") result'.output) \idx err ->
    case jsonParser err >>= CA.decode psaResultCodec >>> lmap CA.printJsonDecodeError of
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

  if Array.all identity arrErrorsIsEmpty then do
    logSuccess "Build succeeded."
  else do
    for_ result'.err $ logDebug <<< show
    die [ "Failed to build." ]

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
     , psaCliFlags :: PsaOutputOptions
     , workspaceOptions :: WorkspacePsaOutputOptions
     }
  -> Array (Effect (Array (String -> Maybe PathDecision)))
toPathDecisions { allDependencies, psaCliFlags, workspaceOptions } = do
  let
    censorAll = eq (Just CensorAllWarnings) $ workspaceOptions.censorLibWarnings
    censorCodes = maybe Set.empty NonEmptySet.toSet $ workspaceOptions.censorLibCodes
    filterCodes = maybe Set.empty NonEmptySet.toSet $ workspaceOptions.filterLibCodes
  (Map.toUnfoldable allDependencies :: Array _) <#> \dep -> do
    case snd dep of
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
              , censorAll
              , censorCodes
              , filterCodes
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
        , censorAll: eq (Just CensorAllWarnings) $ package.build >>= _.censor_project_warnings
        , censorCodes: maybe Set.empty NonEmptySet.toSet $ package.build >>= _.censor_project_codes
        , filterCodes: maybe Set.empty NonEmptySet.toSet $ package.build >>= _.filter_project_codes
        }
    , toPathDecision
        { pathIsFromPackage: isJust <<< String.stripPrefix (String.Pattern testPath)
        , pathType: IsSrc
        , strict: false
        , censorAll: false
        , censorCodes: Set.empty
        , filterCodes: Set.empty
        }
    ]

toPathDecision
  :: { pathIsFromPackage :: String -> Boolean
     , pathType :: PsaPathType
     , strict :: Boolean
     , censorAll :: Boolean
     , censorCodes :: Set ErrorCode
     , filterCodes :: Set ErrorCode
     }
  -> String
  -> Maybe PathDecision
toPathDecision options pathToFile = do
  Alternative.guard $ options.pathIsFromPackage pathToFile
  pure
    { pathType: options.pathType
    , shouldPromoteWarningToError: options.strict
    , shouldShowError: \code ->
        not options.censorAll
          && (Set.isEmpty options.filterCodes || Set.member code options.filterCodes)
          && (Set.isEmpty options.censorCodes || not (Set.member code options.censorCodes))
    }
