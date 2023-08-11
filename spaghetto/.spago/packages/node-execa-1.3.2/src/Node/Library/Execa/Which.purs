-- A majority of the below code was ported from this JavaScript library
-- https://github.com/npm/node-which
-- Copyright `node-which` contributors
-- ISC License: https://opensource.org/license/isc/
module Node.Library.Execa.Which where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.Library.Execa.IsExe (defaultIsExeOptions, isExe, isExeSync)
import Node.Path as Path
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Process as Process
import Node.Library.Execa.Utils (CustomError, buildCustomError)
import Partial.Unsafe (unsafePartial)

isWindows :: Effect Boolean
isWindows = do
  env <- Process.getEnv
  let osTypeIs x = Just x == Object.lookup "OSTYPE" env
  pure $ platform == Just Win32 || osTypeIs "cygwin" || osTypeIs "msys"

jsColon :: Effect String
jsColon = do
  w <- isWindows
  pure if w then ";" else ":"

type WhichOptions =
  { path :: Maybe String
  , pathExt :: Maybe String
  , colon :: Maybe String
  , all :: Boolean
  }

defaultWhichOptions :: WhichOptions
defaultWhichOptions =
  { path: Nothing
  , pathExt: Nothing
  , colon: Nothing
  , all: false
  }

type WhichPathInfo =
  { pathEnv :: Array String
  , pathExtExe :: String
  , pathExt :: Array String
  }

getNotFoundError :: String -> CustomError (code :: String)
getNotFoundError cmd = buildCustomError ("not found: " <> cmd) { code: "ENOENT" }

getPathInfo :: String -> WhichOptions -> Effect WhichPathInfo
getPathInfo cmd options = do
  -- PureScript implementation note: get all the effectful stuff first
  -- before we use it in the rest of this function for readability.
  cwd <- Process.cwd
  mbPath <- Process.lookupEnv "PATH"
  mbPathExt <- Process.lookupEnv "PATHEXT"
  isWin <- isWindows

  colon <- case options.colon of
    Nothing -> jsColon
    Just c -> pure c

  -- If it has a slash, then we don't bother searching the pathenv.
  -- just check the file itself, and that's it.
  let
    pathEnv
      | test hasPosixSlashRegex cmd || (isWin && test hasWindowsSlashRegex cmd) = [ "" ]
      | otherwise = do
          let
            paths = String.split (Pattern colon) $ fromMaybe "" $ options.path <|> mbPath
          -- windows always checks the cwd first
          if isWin then Array.cons cwd paths else paths

    pathExtExe
      -- PureScript note: small attempt at fixing bug
      -- https://github.com/npm/node-which/issues/57
      | isWin = fromMaybe ".EXE;.CMD;.BAT;.exe;.cmd;.bat" $ options.pathExt <|> mbPathExt
      | otherwise = ""

    pathExt
      | isWin = String.split (Pattern colon) pathExtExe
      | otherwise = [ "" ]
  pure { pathEnv, pathExt, pathExtExe }
  where
  hasPosixSlashRegex = unsafeRegex """\/""" noFlags
  hasWindowsSlashRegex = unsafeRegex """\\""" noFlags

which :: String -> WhichOptions -> Aff (Either (CustomError (code :: String)) (NonEmptyArray String))
which cmd options = do
  { pathEnv, pathExt, pathExtExe } <- liftEffect $ getPathInfo cmd options
  go pathEnv pathExt pathExtExe
  where
  go pathEnv pathExt pathExtExe = step [] 0
    where
    pathEnvLen = Array.length pathEnv
    pathExtLen = Array.length pathExt

    step :: Array String -> Int -> Aff (Either (CustomError (code :: String)) (NonEmptyArray String))
    step found i
      | i == pathEnvLen =
          pure $ note (getNotFoundError cmd) $ NEA.fromArray found
      | otherwise = do
          let
            ppRaw = unsafePartial fromJust $ pathEnv Array.!! i
            pathPart
              | test quotedRegex ppRaw = SCU.slice 1 (-1) ppRaw
              | otherwise = ppRaw
            pCmd = Path.concat [ pathPart, cmd ]
            p
              | not $ String.null pathPart
              , test dotSlashRegex cmd = SCU.slice 0 2 cmd <> pCmd
              | otherwise = pCmd
          subStep found p i 0

    subStep found p i ii
      | ii == pathExtLen = step found $ i + 1
      | otherwise = do
          let
            ext = unsafePartial $ fromJust $ pathExt Array.!! ii
            pExt = p <> ext
          Tuple mbErr is <- isExe pExt (defaultIsExeOptions { pathExt = Just pathExtExe })
          if (isNothing mbErr && is) then do
            if options.all then do
              subStep (Array.snoc found pExt) p i $ ii + 1
            else do
              pure $ Right $ NEA.singleton pExt
          else do
            subStep found p i $ ii + 1

whichSync :: String -> WhichOptions -> Effect (Either (CustomError (code :: String)) (NonEmptyArray String))
whichSync cmd options = do
  { pathEnv, pathExt, pathExtExe } <- getPathInfo cmd options
  go pathEnv pathExt pathExtExe
  where
  go pathEnv pathExt pathExtExe = tailRecM loop { found: [], outerLoopIdx: 0, innerLoop: Nothing }
    where
    loop acc@{ found, outerLoopIdx, innerLoop } =
      case innerLoop of
        -- outer for loop
        Nothing -> case pathEnv Array.!! outerLoopIdx of
          Nothing ->
            pure $ Done $ Left $ getNotFoundError cmd
          Just ppRaw -> do
            let
              pathPart
                | test quotedRegex ppRaw = SCU.slice 1 (-1) ppRaw
                | otherwise = ppRaw
              pCmd = Path.concat [ pathPart, cmd ]
              p
                | not $ String.null pathPart
                , test dotSlashRegex cmd = SCU.slice 0 2 cmd <> pCmd
                | otherwise = pCmd
            pure $ Loop $ acc { innerLoop = Just { p, j: 0 } }

        -- inner for loop
        Just { p, j } -> case pathExt Array.!! j of
          Nothing ->
            pure $ Loop $ acc { outerLoopIdx = outerLoopIdx + 1, innerLoop = Nothing }
          Just pExt -> do
            let cur = p <> pExt
            eOrB <- isExeSync cur (defaultIsExeOptions { pathExt = Just pathExtExe })
            case eOrB of
              Right is
                | is && not options.all ->
                    pure $ Done $ Right $ NEA.singleton cur
                | is ->
                    pure $ Loop $ acc { found = Array.snoc found cur, innerLoop = Just { p, j: j + 1 } }
              _ ->
                pure $ Loop $ acc { innerLoop = Just { p, j: j + 1 } }

-- /^".*"$/
quotedRegex :: Regex
quotedRegex = unsafeRegex ("^" <> "\"" <> ".*" <> "\"" <> "$") noFlags

-- Technically, /^\.[\\\/]/
-- but the extra `\` seems to be unnecessary escaping due to square bracket usage
dotSlashRegex :: Regex
dotSlashRegex = unsafeRegex """^\.[\/]""" noFlags
