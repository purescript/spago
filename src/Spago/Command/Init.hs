module Spago.Command.Init ( initProject ) where

import           Spago.Prelude
import           Spago.Env

import qualified Spago.Config             as Config
import qualified Spago.Dhall              as Dhall
import qualified Spago.Messages           as Messages
import qualified Spago.PackageSet         as PackageSet
import qualified Spago.Templates          as Templates
import qualified Spago.RunEnv             as Run


-- | Init a new Spago project:
--   - create `packages.dhall` to manage the package set, overrides, etc
--   - create `spago.dhall` to manage project config: name, deps, etc
--   - create an example `src` folder (if needed)
--   - create an example `test` folder (if needed)
initProject
  :: HasEnv env
  => Force -> Dhall.TemplateComments -> Maybe Text
  -> RIO env Config
initProject force comments tag = do
  logInfo "Initializing a sample project or migrating an existing one.."

  -- packages.dhall and spago.dhall overwrite can be forced
  PackageSet.makePackageSetFile force comments

  -- Use the specified version of the package set (if specified).
  -- Otherwise, get the latest version of the package set if possible
  Run.withPursEnv NoPsa $ do
    PackageSet.updatePackageSetVersion tag

  config <- Config.makeConfig force comments

  -- If these directories (or files) exist, we skip copying "sample sources"
  -- Because you might want to just init a project with your own source files,
  -- or just migrate a psc-package project
  whenDirNotExists "src" $ do
    copyIfNotExists "src/Main.purs" Templates.srcMain

  whenDirNotExists "test" $ do
    copyIfNotExists "test/Main.purs" Templates.testMain

  copyIfNotExists ".gitignore" Templates.gitignore

  copyIfNotExists ".purs-repl" Templates.pursRepl

  logInfo "Set up a local Spago project."
  logInfo "Try running `spago build`"
  pure config

  where
    whenDirNotExists dir action = do
      let dirPath = pathFromText dir
      dirExists <- testdir dirPath
      case dirExists of
        True -> logInfo $ display $ Messages.foundExistingDirectory dir
        False -> do
          mktree dirPath
          action

    copyIfNotExists dest srcTemplate = do
      testfile dest >>= \case
        True  -> logInfo $ display $ Messages.foundExistingFile dest
        False -> writeTextFile dest srcTemplate