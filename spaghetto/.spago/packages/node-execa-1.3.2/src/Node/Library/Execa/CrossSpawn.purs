-- A majority of the below code was ported from this JavaScript library
-- https://github.com/moxystudio/node-cross-spawn
-- Copyright `node-cross-spawn` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.CrossSpawn
  ( parse
  , CrossSpawnConfig
  , CrossSpawnOptions
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.Foldable (for_)
import Data.Function (applyN)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (toLower)
import Data.String.Regex (test)
import Data.String.Regex as StringRegex
import Data.String.Regex.Flags (global, ignoreCase, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (try)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS (FileFlags(..))
import Node.FS.Sync as FS
import Node.Library.Execa.ShebangCommand (shebangCommand)
import Node.Library.Execa.Which (defaultWhichOptions)
import Node.Library.Execa.Which as Which
import Node.Path (FilePath, normalize)
import Node.Path as Path
import Node.Platform (Platform(..))
import Node.Process (lookupEnv, platform)
import Node.Process as Process
import Node.Library.Execa.Utils (bracketEffect)

isWindows :: Boolean
isWindows = platform == Just Win32

type CrossSpawnErrorRows =
  ( code :: String
  , errNo :: String
  , systemCall :: String
  , path :: String
  , spawnArgs :: Array String
  )

type CrossSpawnOptions =
  { shell :: Maybe String
  , env :: Maybe (Object String)
  , cwd :: Maybe String
  , windowsEnableCmdEcho :: Boolean
  , windowsVerbatimArguments :: Maybe Boolean
  }

type CrossSpawnConfig =
  { command :: String
  , args :: Array String
  , options :: CrossSpawnOptions
  , file :: Maybe FilePath
  , original :: { command :: String, args :: Array String }
  }

parse :: String -> Array String -> CrossSpawnOptions -> Effect CrossSpawnConfig
parse command args options = do
  if (not isWindows) then do
    pure initParseRec
  else do
    parseWindows
  where
  initParseRec =
    { command
    , args
    , options
    , file: Nothing
    , original:
        { command
        , args
        }
    }
  parseWindows
    | isJust options.shell = pure initParseRec
    | otherwise = parseNonShell initParseRec

  parseNonShell :: CrossSpawnConfig -> Effect CrossSpawnConfig
  parseNonShell parseRec = do
    -- Detect & add support for shebangs
    Tuple rec1 mbCommandFile <- detectShebang parseRec
    -- We don't need a shell if the command filename is an executable
    let needsShell = not <<< test isExecutableRegex
    case mbCommandFile of
      -- If a shell is required, use cmd.exe and take care of escaping everything correctly
      Just commandFile | needsShell commandFile -> do
        -- Need to double escape meta chars if the command is a cmd-shim located in `node_modules/.bin/`
        -- The cmd-shim simply calls execute the package bin file with NodeJS, proxying any argument
        -- Because the escape of metachars with ^ gets interpreted when the cmd.exe is first called,
        -- we need to double escape them
        let needsDoubleEscapeChars = test isCommandShimRegex commandFile
        comSpec <- fromMaybe "cmd.exe" <$> lookupEnv "comspec"
        pure $ rec1
          { args =
              -- PureScript note: This fix is done in `execa` since
              -- both `cross-spawn` and `Node` don't enable it by default.
              -- But I'm doing it here and exposing an option to re-enable it.
              -- See 
              -- - https://github.com/sindresorhus/execa/issues/116
              -- - https://github.com/moxystudio/node-cross-spawn/issues/135
              -- - https://github.com/nodejs/node/issues/27120
              (guard (toLower comSpec == "cmd.exe" && not parseRec.options.windowsEnableCmdEcho) $> "/q")
                <>
                  [ "/d" -- ignore Autorun
                  , "/s" -- strip wrapping `"` from command line
                  , "/c" -- invoke and exit
                  , wrapInDoubleQuotes
                      $ Array.intercalate " "
                      $ Array.cons (escapeCommand $ normalize rec1.command)
                      $ rec1.args <#> escapeArgument needsDoubleEscapeChars
                  ]
          , command = comSpec
          -- Tell node's spawn that the arguments are already escaped
          , options = rec1.options { windowsVerbatimArguments = Just true }
          }
      _ ->
        pure rec1

  detectShebang :: CrossSpawnConfig -> Effect (Tuple CrossSpawnConfig (Maybe String))
  detectShebang parseRec = do
    mbFile <- resolveCommand parseRec
    case mbFile of
      Nothing -> pure $ Tuple (parseRec { file = mbFile }) mbFile
      Just file -> do
        mbShebang <- readShebang file
        case mbShebang of
          Nothing -> pure $ Tuple (parseRec { file = mbFile }) mbFile
          Just shebang -> do
            let
              rec1 = parseRec
                { file = mbFile
                , args = Array.cons file parseRec.args
                , command = shebang
                }
            newCommand <- resolveCommand rec1
            pure $ Tuple rec1 newCommand

  resolveCommand :: CrossSpawnConfig -> Effect (Maybe String)
  resolveCommand parseRec = do
    env <- case parseRec.options.env of
      Nothing -> Process.getEnv
      Just a -> pure a
    resolved <- withOptionsCwdIfNeeded parseRec.options.cwd \_ -> do
      map join $ for (Object.lookup "PATH" env) \envPath -> do
        let getFirst = either (const Nothing) (Just <<< NEA.head)
        attempt1 <- map getFirst $ Which.whichSync command $ defaultWhichOptions { path = Just envPath, pathExt = Just Path.delimiter }
        if isJust attempt1 then do
          pure attempt1
        else do
          map getFirst $ Which.whichSync command $ defaultWhichOptions { path = Just envPath }
    case parseRec.options.cwd, resolved of
      Just cwd', Just resolved' ->
        Just <$> Path.resolve [ cwd' ] resolved'
      Nothing, Just resolved' ->
        Just <$> Path.resolve [ "" ] resolved'
      _, _ ->
        pure Nothing
    where
    -- switch into options' `cwd` if defined since 'which' does not support custom `cwd`
    withOptionsCwdIfNeeded :: forall b. Maybe String -> ({ cwd :: String, hasChdir :: Boolean } -> Effect b) -> Effect b
    withOptionsCwdIfNeeded optionsCwd = bracketEffect open close
      where
      open = do
        cwd <- Process.cwd
        hasChdir <- processHasChdir
        for_ (guard hasChdir *> optionsCwd) \optionCwd -> do
          Process.chdir optionCwd
        pure { cwd, hasChdir }

      close { cwd, hasChdir } = do
        for_ (guard hasChdir *> optionsCwd) \_ -> do
          Process.chdir cwd

  readShebang cmd = do
    -- read first 150 bytes of file
    let size = 150
    buf <- Buffer.create size
    void $ try $ bracketEffect (FS.fdOpen cmd R Nothing) (FS.fdClose) \fd ->
      FS.fdRead fd buf 0 size (Just 0)
    firstLine <- Buffer.toString UTF8 buf
    pure $ shebangCommand firstLine

  isExecutableRegex = unsafeRegex """\.(?:com|exe)$""" ignoreCase

  isCommandShimRegex = unsafeRegex """node_modules[\\/].bin[\\/][^\\/]+\.cmd$""" ignoreCase

  escapeCommand = StringRegex.replace metaCharsRegex "^$1"

  -- See http://www.robvanderwoude.com/escapechars.php
  metaCharsRegex = unsafeRegex """([()\][%!^"`<>&|;, *?])""" global

  -- Algorithm below is based on https://qntm.org/cmd
  -- Note on PureScript implementation:
  --    A `"\""` is appended rather than defined inline
  --    to fix syntax highlighting in PureScript
  escapeArgument doubleEscapeMetaChars =
    -- Sequence of backslashes followed by a double quote:
    -- double up all the backslashes and escape the double quote

    (StringRegex.replace backSlashSequenceThenDoubleQuoteRegex ("""$1$1\""" <> "\""))
      -- Sequence of backslashes followed by the end of the string
      -- (which will become a double quote later):
      -- double up all the backslashes
      >>> (StringRegex.replace endOfStringRegex "$1$1")
      -- All other backslashes occur literally
      -- Quote the whole thing:
      >>> wrapInDoubleQuotes
      -- Escape meta chars (double escape if needed)
      >>> applyN (StringRegex.replace metaCharsRegex "^$1") escapeCount
    where
    escapeCount = if doubleEscapeMetaChars then 2 else 1
    backSlashSequenceThenDoubleQuoteRegex =
      unsafeRegex ("""(\\*)""" <> "\"") global
    endOfStringRegex =
      unsafeRegex """(\\*)$""" noFlags

  wrapInDoubleQuotes s = "\"" <> s <> "\""

foreign import processHasChdir :: Effect Boolean
