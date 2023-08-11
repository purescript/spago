-- | This module defines a data type representing ANSI escape codes, as well as
-- | functions for serialising them as Strings.
module Ansi.Codes where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Show.Generic (genericShow)

-- | The prefix for all escape codes.
prefix :: String
prefix = "\x1b["

-- | The suffix for escape codes; note that this is only required for colors.
colorSuffix :: String
colorSuffix = "m"

-- | An ANSI escape code. Not all sequences are implemented.
-- | See: <https://en.wikipedia.org/wiki/ANSI_escape_code>.
data EscapeCode
  = Up Int
  | Down Int
  | Forward Int
  | Back Int
  | NextLine Int
  | PreviousLine Int
  | HorizontalAbsolute Int
  | Position Int Int
  | EraseData EraseParam
  | EraseLine EraseParam
  | ScrollUp Int
  | ScrollDown Int
  | Graphics (NonEmptyList GraphicsParam)
  | SavePosition
  | RestorePosition
  | QueryPosition
  | HideCursor
  | ShowCursor

derive instance genericEscapeCode :: Generic EscapeCode _
derive instance eqEscapeCode :: Eq EscapeCode
derive instance ordEscapeCode :: Ord EscapeCode
instance showEscapeCode :: Show EscapeCode where
  show = genericShow

-- | Convert an escape code to the form recognised by terminals.
escapeCodeToString :: EscapeCode -> String
escapeCodeToString = (prefix <> _) <<< go
  where
  go c =
    case c of
      Up n                 -> show n <> "A"
      Down n               -> show n <> "B"
      Forward n            -> show n <> "C"
      Back n               -> show n <> "D"
      NextLine n           -> show n <> "E"
      PreviousLine n       -> show n <> "F"
      HorizontalAbsolute n -> show n <> "G"
      Position x y         -> show x <> ";" <> show y <> "H"
      EraseData p          -> ep p <> "J"
      EraseLine p          -> ep p <> "K"
      ScrollUp n           -> show n <> "S"
      ScrollDown n         -> show n <> "T"
      Graphics ps          -> intercalate ";" (map gp ps) <> colorSuffix
      SavePosition         -> "s"
      RestorePosition      -> "u"
      QueryPosition        -> "6n"
      HideCursor           -> "?25l"
      ShowCursor           -> "?25h"

  ep = eraseParamToString
  gp = graphicsParamToString

-- | Specifies how much text to erase.
-- |
-- | * ToEnd: erase from the cursor to the end of the line or screen.
-- | * FromBeginning: erase to the cursor from the beginning of the line or
-- |    screen.
-- | * Entire: erase the entire line or screen.
data EraseParam
  = ToEnd
  | FromBeginning
  | Entire

derive instance genericEraseParam :: Generic EraseParam _
derive instance eqEraseParam :: Eq EraseParam
derive instance ordEraseParam :: Ord EraseParam
instance showEraseParam :: Show EraseParam where
  show = genericShow

eraseParamToString :: EraseParam -> String
eraseParamToString ep =
  case ep of
    ToEnd         -> "0"
    FromBeginning -> "1"
    Entire        -> "2"

-- | A graphics parameter, controls how text appears; for example, bold,
-- | underlined, foreground color, background color.
data GraphicsParam
  = Reset
  | PMode RenderingMode
  | PForeground Color
  | PBackground Color

derive instance genericGraphicsParam :: Generic GraphicsParam _
derive instance eqGraphicsParam :: Eq GraphicsParam
derive instance ordGraphicsParam :: Ord GraphicsParam
instance showGraphicsParam :: Show GraphicsParam where
  show = genericShow

graphicsParamToString :: GraphicsParam -> String
graphicsParamToString gp =
  case gp of
    Reset         -> "0"
    PMode m       -> show (codeForRenderingMode m)
    PForeground c -> show (colorCode c)
    PBackground c -> show (colorCode c + 10)

data RenderingMode
  = Bold
  | Dim
  | Italic
  | Underline
  | Inverse
  | Strikethrough

derive instance genericRenderingMode :: Generic RenderingMode _
derive instance eqRenderingMode :: Eq RenderingMode
derive instance ordRenderingMode :: Ord RenderingMode
instance showRenderingMode :: Show RenderingMode where
  show = genericShow

codeForRenderingMode :: RenderingMode -> Int
codeForRenderingMode m =
  case m of
    Bold -> 1
    Dim -> 2
    Italic -> 3
    Underline -> 4
    Inverse -> 7
    Strikethrough -> 9

-- | The standard set of 16 ANSI colors.
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite

derive instance genericColor :: Generic Color _
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color
instance showColor :: Show Color where
  show = genericShow

colorCode :: Color -> Int
colorCode c =
  case c of
    Black -> 30
    Red -> 31
    Green -> 32
    Yellow -> 33
    Blue -> 34
    Magenta -> 35
    Cyan -> 36
    White -> 37
    BrightBlack -> 90
    BrightRed -> 91
    BrightGreen -> 92
    BrightYellow -> 93
    BrightBlue -> 94
    BrightMagenta -> 95
    BrightCyan -> 96
    BrightWhite -> 97
