{-

Main module for `spago-curator` executable.

Contains commands to help curate various spago metadata

-}

module Curator (main) where

import Spago.Prelude

import qualified System.Environment as Env
import qualified Turtle             as CLI
import qualified GHC.IO.Encoding
import qualified Data.Text as Text

import qualified Curator.Metadata as Meta

data Command
  = IndexGitHubMeta


main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- Read GitHub Auth Token
  token <- fmap Text.pack $ Env.getEnv "SPACCHETTIBOTTI_TOKEN"

  command <- CLI.options "Spago Curator" parser

  case command of
    IndexGitHubMeta -> Meta.indexGitHub token

  where
    parser
      = CLI.subcommand "index-github-meta" "Download metadata about packages commits and tags from GitHub" $ pure IndexGitHubMeta
