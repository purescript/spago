module Spago.Command.Path (showPaths, getOutputPath, findFlag) where

import Spago.Prelude
import Spago.Env

import qualified Data.Text            as Text
import qualified System.IO            as Sys


showPaths
  :: (HasLogFunc env, HasGlobalCache env)
  => BuildOptions
  -> Maybe PathType
  -> RIO env ()
showPaths buildOptions whichPaths =
  case whichPaths of
    (Just PathOutput) -> outputStr (getOutputPath buildOptions)
    (Just PathGlobalCache) -> view globalCacheL >>= outputStr 
    Nothing -> do
      let showPath (a,b) = output (a <> ": " <> b)
      getAllPaths buildOptions >>= traverse_ showPath


getAllPaths
  :: (HasLogFunc env, HasGlobalCache env)
  => BuildOptions
  -> RIO env [(Text, Text)]
getAllPaths buildOptions = do
  globalCache <- view globalCacheL
  pure
    [ ("output", Text.pack (getOutputPath buildOptions))
    , ("global-cache", Text.pack globalCache)
    ]


-- | Find the output path for purs compiler
getOutputPath
  :: BuildOptions
  -> Sys.FilePath
getOutputPath buildOpts = do
  case findFlag 'o' "output" (pursArgs buildOpts) of
    Nothing -> "output"
    Just path -> Text.unpack path


-- See tests in: test/Spago/Command/PathSpec.hs
-- ["-o", "something"]
-- ["--output", "something"]
-- ["--output something"]
-- [" -o something"]
-- ["--output=something"]

-- | Try to find the content of a certain flag in a list of PursArgs
findFlag :: Char -> Text -> [PursArg] -> Maybe Text
findFlag char string = \case
  (x:xs) -> if isFlag x
              then case xs of
                (y:_) -> Just (unPursArg y)
                _ -> Nothing
              else if hasFlag x
                then case Text.words (unPursArg x) of
                  [word] -> case Text.split (=='=') word of
                    [_,value] -> Just value
                    _           -> Nothing
                  (_:value:_) -> Just value
                  _ -> Nothing
                else findFlag char string xs
  _ -> Nothing
  where
    isFlag :: PursArg -> Bool
    isFlag (PursArg word)
      =  word == (Text.pack ['-', char])
      || word == ("--" <> string)
    hasFlag :: PursArg -> Bool
    hasFlag (PursArg a)
      =  firstWord == (Text.pack ['-', char])
      || firstWord == ("--" <> string)
        where
          firstWord
            = fromMaybe "" $ case Text.words a of
                []       -> Nothing
                [word]   -> case Text.split (=='=') word of
                  [one]       -> Just one
                  [key,_]     -> Just key
                  _           -> Nothing
                (word:_) -> Just word
