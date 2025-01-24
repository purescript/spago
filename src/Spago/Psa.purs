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
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Either (blush)
import Data.Map as Map
import Data.Set as Set
import Data.String as Str
import Data.String as String
import Data.Tuple as Tuple
import Effect.Ref as Ref
import Foreign.Object as FO
import JSON as JSON
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding as Encoding
import Node.FS.Aff as FSA
import Spago.Cmd as Cmd
import Spago.Config (Package(..), PackageMap, WorkspacePackage)
import Spago.Config as Config
import Spago.Core.Config (CensorBuildWarnings(..), WarningCensorTest(..))
import Spago.Core.Config as Core
import Spago.Log (prepareToDie)
import Spago.Path as Path
import Spago.Psa.Output (buildOutput)
import Spago.Psa.Printer (printDefaultOutputToErr, printJsonOutputToOut)
import Spago.Psa.Types (ErrorCode, PathDecision, PsaArgs, PsaOutputOptions, PsaPathType(..), PsaEnv, psaResultCodec)
import Spago.Purs as Purs

defaultStatVerbosity :: Core.StatVerbosity
defaultStatVerbosity = Core.CompactStats

psaCompile :: ∀ a. RootPath -> Set.Set LocalPath -> Array String -> PsaArgs -> Spago (PsaEnv a) Boolean
psaCompile root globs pursArgs psaArgs = do
  purs <- Purs.compile root globs (Array.snoc pursArgs "--json-errors")
  let
    resultStdout = Cmd.getStdout purs
    print' = if psaArgs.jsonErrors then printJsonOutputToOut else printDefaultOutputToErr psaArgs

  errors <- for (Str.split (Str.Pattern "\n") resultStdout) \err -> runExceptT do
    -- If we can't decode the error, then there's likely a codec issue on Spago's side.
    -- So, this shouldn't fail the build.
    out <- ExceptT $ pure $ JSON.parse err >>= CJ.decode psaResultCodec >>> lmap CJ.DecodeError.print
    files <- liftEffect $ Ref.new FO.empty
    out' <- lift $ buildOutput (loadLines files) psaArgs out
    liftEffect (print' out') $> FO.isEmpty out'.stats.allErrors

  let
    noErrors = Array.all (either (const true) identity) errors
    failedToDecodeMsg (idx /\ err) =
      Array.intercalate "\n"
        [ "Failed to decode PsaResult at index '" <> show idx <> "': " <> err
        , "Json was: " <> err
        ]
    failedToDecode = failedToDecodeMsg <$> Array.catMaybes (Array.mapWithIndex (\idx e -> (idx /\ _) <$> blush e) errors)

  case Cmd.exit purs, noErrors of
    Normally 0, true ->
      for failedToDecode logWarn
        *> logSuccess "Build succeeded."
        $> true
    _, true ->
      prepareToDie [ "purs exited with non-ok status code: " <> show (Cmd.exit purs) ]
        $> false
    _, _ ->
      for (blush purs) (logDebug <<< Cmd.printExecResult)
        *> prepareToDie [ "Failed to build." ]
        $> false

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
  :: { rootPath :: RootPath
     , allDependencies :: PackageMap
     , selectedPackages :: Array WorkspacePackage
     , psaCliFlags :: PsaOutputOptions
     , censorLibWarnings :: Maybe Core.CensorBuildWarnings
     , censorProjectWarnings :: Maybe Core.CensorBuildWarnings
     }
  -> Array (Effect (Array (LocalPath -> Maybe PathDecision)))
toPathDecisions { rootPath, allDependencies, selectedPackages, psaCliFlags, censorLibWarnings, censorProjectWarnings } =
  projectDecisions <> dependencyDecisions
  where
  projectDecisions = selectedPackages <#> \selected -> toWorkspacePackagePathDecision { selected, psaCliFlags, censorProjectWarnings }

  dependencyDecisions =
    map toDependencyDecision
      $ Map.toUnfoldable
      -- Remove workspace packages that are dependencies of some other workspace package
      -- so that we don't add their entries twice
      $ Map.filterKeys (\pkgName -> not $ Set.member pkgName pkgsInProject) allDependencies

  pkgsInProject :: Set PackageName
  pkgsInProject = foldMap (\p -> Set.singleton p.package.name) selectedPackages

  toDependencyDecision :: Tuple PackageName Package -> Effect (Array (LocalPath -> Maybe PathDecision))
  toDependencyDecision dep = case snd dep of
    WorkspacePackage p ->
      toWorkspacePackagePathDecision
        { selected: p
        , psaCliFlags
        , censorProjectWarnings
        }
    _ -> do
      let pkgLocation = Tuple.uncurry (Config.getLocalPackageLocation rootPath) dep
      pure
        [ toPathDecision
            { pathIsFromPackage: (pkgLocation `Path.isPrefixOf` _)
            , pathType: IsLib
            , strict: false
            , censorWarnings: censorLibWarnings
            }
        ]

toWorkspacePackagePathDecision
  :: { selected :: WorkspacePackage
     , psaCliFlags :: PsaOutputOptions
     , censorProjectWarnings :: Maybe Core.CensorBuildWarnings
     }
  -> Effect (Array (LocalPath -> Maybe PathDecision))
toWorkspacePackagePathDecision { selected: { path, package }, psaCliFlags, censorProjectWarnings } = do
  let srcPath = path </> "src"
  let testPath = path </> "test"
  pure
    [ toPathDecision
        { pathIsFromPackage: (srcPath `Path.isPrefixOf` _)
        , pathType: IsSrc
        , strict: fromMaybe false $ psaCliFlags.strict <|> (package.build >>= _.strict)
        , censorWarnings: (package.build >>= _.censorProjectWarnings) <|> censorProjectWarnings
        }
    , toPathDecision
        { pathIsFromPackage: (testPath `Path.isPrefixOf` _)
        , pathType: IsSrc
        , strict: fromMaybe false $ psaCliFlags.strict <|> (package.test >>= _.strict)
        , censorWarnings: (package.test >>= _.censorTestWarnings) <|> censorProjectWarnings
        }
    ]

toPathDecision
  :: { pathIsFromPackage :: LocalPath -> Boolean
     , pathType :: PsaPathType
     , strict :: Boolean
     , censorWarnings :: Maybe Config.CensorBuildWarnings
     }
  -> LocalPath
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
      \code msg -> isNothing $ Array.find (\f -> f code msg) tests
