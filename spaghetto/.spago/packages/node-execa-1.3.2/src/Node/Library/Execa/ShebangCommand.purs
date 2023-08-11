-- A majority of the below code was ported from this JavaScript library
-- https://github.com/kevva/shebang-command
-- Copyright `shebang-command` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.ShebangCommand where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

shebangCommand :: String -> Maybe String
shebangCommand firstLineOfFile = do
  parts <- match shebangRegex firstLineOfFile
  let
    extractBinary = Array.last <<< String.split (String.Pattern "/")

  everythingAfterShebang <- NEA.head parts
  case String.split (String.Pattern " ") everythingAfterShebang of
    [ pathOnly ] -> do
      binary <- extractBinary pathOnly
      binary <$ guard (binary /= "env")
    [ path, argument ] -> do
      binary <- extractBinary path
      pure
        if binary == "env" then
          argument
        else
          binary <> " " <> argument
    _ -> Nothing
  where
  shebangRegex :: Regex
  shebangRegex = unsafeRegex """^#! ?(.*)""" noFlags
