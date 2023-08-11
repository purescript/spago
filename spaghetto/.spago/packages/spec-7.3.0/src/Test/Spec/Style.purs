module Test.Spec.Style where

import Prelude

import Ansi.Codes (GraphicsParam, escapeCodeToString)
import Ansi.Codes as AnsiCode
import Data.Array as Array
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CodeUnits

type Style = Array GraphicsParam

styled :: Style -> String -> String
styled as str = do
  case NEL.fromFoldable as of
    Nothing -> do
      str
    Just as' -> do
      escapeCodeToString (AnsiCode.Graphics as')
       <> str
       <> escapeCodeToString (AnsiCode.Graphics $ NEL.singleton AnsiCode.Reset)

red :: Style
red = [AnsiCode.PForeground AnsiCode.Red]

green :: Style
green = [AnsiCode.PForeground AnsiCode.Green]

yellow :: Style
yellow = [AnsiCode.PForeground AnsiCode.Yellow]

cyan :: Style
cyan = [AnsiCode.PForeground AnsiCode.Cyan]

dim :: Style
dim = [AnsiCode.PMode AnsiCode.Dim]

bold :: Style
bold = [AnsiCode.PMode AnsiCode.Bold]

magenta :: Style
magenta = [AnsiCode.PForeground AnsiCode.Magenta]

indent :: Int -> String
indent i = CodeUnits.fromCharArray $ Array.replicate i ' '
