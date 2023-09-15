module Subpackage.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Path as Path
import Node.Process as Process

main :: Effect Unit
main = do
  pwd <- Process.cwd
  pwd' <- Path.resolve [] pwd
  log pwd'
