-- A majority of the below code was ported from this JavaScript library
-- https://github.com/sindresorhus/npm-run-path
-- Copyright `npm-run-path` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.NpmRunPath where

import Prelude

import Control.Alt (alt)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Path as Path
import Node.Process as Process

-- | `cwd`: the working directory.
-- | `path`: the PATH to be appended. Set it to an empty string to exclude the default PATH
-- | `execPath`: the pathe to the current Node.js executable.
-- |    Its directory is pushed to the front of PATH.
-- |    It can be either an absolute path or a path relative to the `cwd` option.
type NpmRunPathOptions =
  { cwd :: Maybe String
  , path :: Maybe String
  , execPath :: Maybe String
  }

defaultNpmRunPathOptions :: NpmRunPathOptions
defaultNpmRunPathOptions = mempty

npmRunPath :: NpmRunPathOptions -> Effect String
npmRunPath initialOptions = do
  processCwd <- Process.cwd
  processPath <- Process.lookupEnv "PATH"
  processExecPath <- Process.execPath
  let
    options =
      { cwd: fromMaybe processCwd initialOptions.cwd
      , path: initialOptions.path `alt` processPath
      , execPath: fromMaybe processExecPath initialOptions.execPath
      }
  -- PureScript note: we don't currently support `URL`s as a possible
  -- `cwd` value
  cwdPath <- Path.resolve [] options.cwd

  go options [] Nothing cwdPath
  where
  go :: _ -> Array String -> _ -> String -> Effect String
  go options result previous cwdPath = case previous of
    Just p | p == cwdPath -> do
      -- Ensure the running `node` binary is used
      nodeBinaryPath <- Path.resolve [ cwdPath, options.execPath ] ".."
      pure $ Array.intercalate Path.delimiter
        (result <> [ nodeBinaryPath ] <> foldMap Array.singleton options.path)

    _ -> do
      let
        nextResult = Path.concat [ cwdPath, "node_modules/.bin" ]
        nextPrevious = Just cwdPath
      nextCwdPath <- Path.resolve [ cwdPath ] ".."
      go options (Array.snoc result nextResult) nextPrevious nextCwdPath

-- | Modifies the given environment's `PATH` key
-- | so that any locally installed NPM binaries are executed.
-- | Works by recursively walking up the file system
-- | and prepending `node_modules/.bin` to the `PATH`.
-- |
-- | In other words, given a `cwd` of `parent1/parent2/root/`,
-- | updates the `PATH` key to:
-- | ```
-- | Array.intercalate Path.delimiter $
-- |   [ "parent1/parent2/root/node_modules/.bin"
-- |   , "parent1/parent2/node_modules/.bin"
-- |   , "parent1/node_modules/.bin"
-- |   ]
-- |   <> (foldMap (\a -> [a]) $ Object.lookup "PATH" $ process.env)
-- | ````
npmRunPathEnv :: Object String -> NpmRunPathOptions -> Effect (Object String)
npmRunPathEnv env options =
  (\p -> Object.insert "PATH" p env) <$> npmRunPath options
