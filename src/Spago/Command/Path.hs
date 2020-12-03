module Spago.Command.Path (showPaths, getOutputPath) where

import Spago.Prelude
import Spago.Env

import qualified Data.Text            as Text
import qualified System.IO            as Sys

import qualified Spago.Purs as Purs


showPaths
  :: (HasGlobalCache env)
  => BuildOptions
  -> Maybe PathType
  -> RIO env ()
showPaths BuildOptions{ pursArgs } whichPaths =
  case whichPaths of
    (Just PathOutput) -> outputStr (getOutputPath pursArgs)
    (Just PathGlobalCache) -> do
      GlobalCache path _ <- view (the @GlobalCache)
      outputStr path
    Nothing -> do
      let showPath (a,b) = output (a <> ": " <> b)
      getAllPaths pursArgs >>= traverse_ showPath


getAllPaths
  :: (HasGlobalCache env)
  => [PursArg]
  -> RIO env [(Text, Text)]
getAllPaths pursArgs = do
  GlobalCache path _ <- view (the @GlobalCache)
  pure
    [ ("output", Text.pack (getOutputPath pursArgs))
    , ("global-cache", Text.pack path)
    ]


-- | Find the output path for purs compiler
getOutputPath
  :: [PursArg]
  -> Sys.FilePath
getOutputPath pursArgs = do
  case Purs.findFlag 'o' "output" pursArgs of
    Nothing -> "output"
    Just path -> Text.unpack path