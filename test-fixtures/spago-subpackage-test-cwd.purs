module Subpackage.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  readTextFile UTF8 filePath >>= log

-- DO NOT EDIT. This value is replaced in the corresponding spago test
filePath :: String
filePath = "FILE_PATH"
