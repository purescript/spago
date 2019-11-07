module Main where

import Prelude

import Test.Hspec.Runner
import qualified Spec
import qualified GHC.IO.Encoding

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  hspecWith defaultConfig Spec.spec
