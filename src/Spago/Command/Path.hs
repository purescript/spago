module Spago.Command.Path (showPaths, getOutputPath) where

import Spago.Prelude
import Spago.Env

import qualified Data.Text            as Text
import qualified System.IO            as Sys


showPaths
  :: (HasLogFunc env)
  => BuildOptions
  -> Maybe PathType
  -> RIO env ()
showPaths buildOptions whichPaths =
  case whichPaths of
    (Just OutputFolder) -> outputStr (getOutputPath buildOptions)
    Nothing -> do
      let showPath (a,b) = output (a <> ": " <> b)
      getAllPaths buildOptions >>= traverse_ showPath


getAllPaths
  :: (HasLogFunc env)
  => BuildOptions
  -> RIO env [(Text, Text)]
getAllPaths buildOptions =
  pure [ ("output", Text.pack (getOutputPath buildOptions)) ]


-- | Find the output path for purs compiler
getOutputPath
  :: BuildOptions
  -> Sys.FilePath
getOutputPath buildOpts = do
  case findFlag 'o' "output" (pursArgs buildOpts) of
    Nothing -> "output"
    Just path -> Text.unpack path


-- TODO tests:
-- ["-o", "something"]
-- ["--output", "something"]
-- ["--output something"]
-- [" -o something"]
-- ["--output=something"]

-- | Try to find the content of a certain flag in a list of PursArgs
findFlag :: Char -> Text -> [PursArg] -> Maybe Text
findFlag char string = \case
  [] -> Nothing
  [_] -> Nothing
  (x:y:xs) -> if isFlag x
              then Just (unPursArg y)
              else findFlag char string (y : xs)
  where 
    isFlag :: PursArg -> Bool
    isFlag (PursArg a)
      =  firstWord == (Text.pack ['-', char])
      || firstWord == ("--" <> string)
        where
          firstWord
            = fromMaybe "" $ case Text.words a of
                []       -> Nothing
                (word:_) -> Just word