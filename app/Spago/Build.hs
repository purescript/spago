module Spago.Build
  ( build
  , test
  , repl
  , bundle
  , makeModule
  , Purs.ExtraArg (..)
  , Purs.ModuleName (..)
  , Purs.SourcePath (..)
  , Purs.TargetPath (..)
  , Purs.WithMain (..)
  ) where

import           Control.Exception (SomeException, try)
import           Data.Maybe        (fromMaybe)
import qualified Data.Text         as Text
import           System.IO         (hPutStrLn)
import qualified Turtle            as T hiding (die, echo)

import qualified Spago.Config      as Config
import qualified Spago.Packages    as Packages
import qualified Spago.Purs        as Purs
import           Spago.Turtle


defaultSourcePaths :: [Purs.SourcePath]
defaultSourcePaths =
  [ Purs.SourcePath "src/**/*.purs"
  , Purs.SourcePath "test/**/*.purs"
  ]

prepareBundleDefaults
  :: Maybe Purs.ModuleName
  -> Maybe Purs.TargetPath
  -> (Purs.ModuleName, Purs.TargetPath)
prepareBundleDefaults maybeModuleName maybeTargetPath = (moduleName, targetPath)
  where
    moduleName = fromMaybe (Purs.ModuleName "Main") maybeModuleName
    targetPath = fromMaybe (Purs.TargetPath "index.js") maybeTargetPath


-- | Build the project with purs, passing through
--   the additional args in the list
build :: (Maybe Int) -> [Purs.SourcePath] -> [Purs.ExtraArg] -> IO ()
build maybeLimit sourcePaths passthroughArgs = do
  config <- Config.ensureConfig
  deps <- Packages.getProjectDeps config
  Packages.fetchPackages maybeLimit deps
  let globs = Packages.getGlobs deps <> defaultSourcePaths <> sourcePaths
  Purs.compile globs passthroughArgs

-- | Start a repl
repl :: [Purs.SourcePath] -> [Purs.ExtraArg] -> IO ()
repl sourcePaths passthroughArgs = do
  config <- Config.ensureConfig
  deps <- Packages.getProjectDeps config
  let globs = Packages.getGlobs deps <> defaultSourcePaths <> sourcePaths
  Purs.repl globs passthroughArgs

-- | Test the project: compile and run the Test.Main
--   (or the provided module name) with node
test :: Maybe Purs.ModuleName -> Maybe Int -> [Purs.SourcePath] -> [Purs.ExtraArg] -> IO ()
test maybeModuleName maybeLimit paths passthroughArgs = do
  build maybeLimit paths passthroughArgs
  T.shell cmd T.empty >>= \case
    T.ExitSuccess   -> echo "Tests succeeded."
    T.ExitFailure n -> die $ "Tests failed: " <> T.repr n
  where
    moduleName = fromMaybe (Purs.ModuleName "Test.Main") maybeModuleName
    cmd = "node -e \"require('./output/" <> Purs.unModuleName moduleName <> "').main()\""

-- | Bundle the project to a js file
bundle :: Purs.WithMain -> Maybe Purs.ModuleName -> Maybe Purs.TargetPath -> IO ()
bundle withMain maybeModuleName maybeTargetPath =
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
  in Purs.bundle withMain moduleName targetPath

-- | Bundle into a CommonJS module
makeModule :: Maybe Purs.ModuleName -> Maybe Purs.TargetPath -> IO ()
makeModule maybeModuleName maybeTargetPath = do
  let (moduleName, targetPath) = prepareBundleDefaults maybeModuleName maybeTargetPath
      jsExport = Text.unpack $ "\nmodule.exports = PS[\""<> Purs.unModuleName moduleName <> "\"];"
  echo "Bundling first..."
  bundle Purs.WithoutMain (Just moduleName) (Just targetPath)
  -- Here we append the CommonJS export line at the end of the bundle
  try (T.with
        (T.appendonly $ T.fromText $ Purs.unTargetPath targetPath)
        ((flip hPutStrLn) jsExport))
    >>= \case
      Right _ -> echo $ "Make module succeeded and output file to " <> Purs.unTargetPath targetPath
      Left (n :: SomeException) -> die $ "Make module failed: " <> T.repr n
