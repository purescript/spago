-- A majority of the below code was ported from this JavaScript library
-- https://github.com/isaacs/isexe
-- Copyright `is-exe` contributors
-- ISC License: https://opensource.org/license/isc/
module Node.Library.Execa.IsExe
  ( isExe
  , isExeSync
  , IsExeOptions
  , defaultIsExeOptions
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (any)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Int.Bits ((.&.), (.|.))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Effect.Exception (try)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.FS.Async as FsAsync
import Node.FS.Stats (Stats(..), isFile, isSymbolicLink)
import Node.FS.Sync as FsSync
import Node.Platform (Platform(..))
import Node.Process (lookupEnv, platform)
import Unsafe.Coerce (unsafeCoerce)

type IsExeOptions =
  { pathExt :: Maybe String
  , uid :: Maybe Int
  , gid :: Maybe Int
  , ignoreErrors :: Boolean
  }

defaultIsExeOptions :: IsExeOptions
defaultIsExeOptions =
  { pathExt: Nothing
  , uid: Nothing
  , gid: Nothing
  , ignoreErrors: false
  }

isExe :: String -> IsExeOptions -> Aff (Tuple (Maybe Error) Boolean)
isExe path options = do
  let
    core = case platform of
      Just Win32 -> coreWindows
      _ -> coreNonWindows

  original <- core.isExe path options
  case fst original of
    Just e
      -- ignore EACCES because that just means we aren't 
      -- allowed to run it
      | "EACCES" <- getErrorCode e
      , true <- options.ignoreErrors ->
          pure $ Tuple Nothing false
    _ -> pure original

isExeSync :: String -> IsExeOptions -> Effect (Either Error Boolean)
isExeSync path options = do
  let
    core = case platform of
      Just Win32 -> coreWindows
      _ -> coreNonWindows
  mbEither <- try $ core.isExeSync path options
  case mbEither of
    Left e
      | true <- options.ignoreErrors
      , "EACCESS" <- getErrorCode e -> pure $ Right false
      | otherwise -> pure $ Left e
    Right b -> pure $ Right b

getErrorCode :: Error -> String
getErrorCode e = ((unsafeCoerce :: Error -> { code :: String }) e).code

type IsExeCore =
  { isExe :: String -> IsExeOptions -> Aff (Tuple (Maybe Error) Boolean)
  , isExeSync :: String -> IsExeOptions -> Effect Boolean
  }

coreWindows :: IsExeCore
coreWindows =
  { isExe: winIsExe, isExeSync: winIsExeSync }
  where
  checkStat :: Stats -> String -> IsExeOptions -> Effect Boolean
  checkStat stat path options
    | not (isSymbolicLink stat)
    , not (isFile stat) = pure false
    | otherwise = checkPathExt path options

  checkPathExt :: String -> IsExeOptions -> Effect Boolean
  checkPathExt path options = do
    mbPathExt <- lookupEnv "PATHEXT"
    case options.pathExt <|> mbPathExt of
      Nothing -> pure true
      Just p -> do
        let
          pathExt :: Array String
          pathExt = String.split (Pattern ";") p
          pathLen = String.length path
          allElemsNonEmpty = isNothing $ Array.find (eq "") pathExt
          pathEndsInExt = isJust $ pathExt # Array.find \p1 -> do
            let
              p2 :: String
              p2 = String.drop (pathLen - String.length p1) path
            eq (String.toLower p1) (String.toLower p2)

        pure $ allElemsNonEmpty || pathEndsInExt

  winIsExe :: String -> IsExeOptions -> Aff (Tuple (Maybe Error) Boolean)
  winIsExe path options = makeAff \cb -> do
    FsAsync.stat path case _ of
      Left err -> cb $ Right $ Tuple (Just err) false
      Right statsObj -> do
        result <- checkStat statsObj path options
        cb $ Right $ Tuple Nothing result
    pure nonCanceler

  winIsExeSync :: String -> IsExeOptions -> Effect Boolean
  winIsExeSync path options = do
    statsObj <- FsSync.stat path
    checkStat statsObj path options

coreNonWindows :: IsExeCore
coreNonWindows =
  { isExe: nonWinIsExe, isExeSync: nonWinIsExeSync }
  where
  nonWinIsExe :: String -> IsExeOptions -> Aff (Tuple (Maybe Error) Boolean)
  nonWinIsExe path options = makeAff \cb -> do
    FsAsync.stat path case _ of
      Left e -> cb $ Right $ Tuple (Just e) false
      Right stats -> do
        b <- checkStat stats options
        cb $ Right $ Tuple Nothing b
    pure nonCanceler

  nonWinIsExeSync :: String -> IsExeOptions -> Effect Boolean
  nonWinIsExeSync path options = do
    stats <- FsSync.stat path
    checkStat stats options

  checkStat :: Stats -> IsExeOptions -> Effect Boolean
  checkStat stat options = do
    b <- checkMode stat options
    pure $ (isFile stat) && b

  checkMode :: Stats -> IsExeOptions -> Effect Boolean
  checkMode (Stats statObj) options = do
    let
      mode = floor statObj.mode
      uid = floor statObj.uid
      gid = floor statObj.gid

    processMbUid <- getUid
    processMbGid <- getGid

    let
      myUid = options.uid <|> processMbUid
      myGid = options.gid <|> processMbGid
      u = 64 -- base-8 binary: 100
      g = 8 -- base-8 binary: 010
      o = 1 -- base-8 binary: 001
      ug = u .|. g

      truthy x = x /= 0

    pure $ any identity
      [ truthy (mode .&. o)
      , truthy (mode .&. g) && maybe false (eq gid) myGid
      , truthy (mode .&. u) && maybe false (eq uid) myUid
      , truthy (mode .&. ug) && maybe false (eq 0) myUid
      ]

foreign import getUidImpl :: EffectFn2 (forall a. Maybe a) (forall a. a -> Maybe a) (Maybe Int)

foreign import getGidImpl :: EffectFn2 (forall a. Maybe a) (forall a. a -> Maybe a) (Maybe Int)

getUid :: Effect (Maybe Int)
getUid = runEffectFn2 getUidImpl Nothing Just

getGid :: Effect (Maybe Int)
getGid = runEffectFn2 getGidImpl Nothing Just
