module Subpackage.SameName.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ packageName

packageName :: String
packageName = "packageA"

