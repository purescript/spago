-- | Convenience functions to simplify outputting ANSI escape codes to
-- | terminals.
module Ansi.Output where

import Prelude

import Data.List.NonEmpty (NonEmptyList)
import Ansi.Codes (Color, EscapeCode(..), GraphicsParam(..), RenderingMode(..), escapeCodeToString)

-- | Wrap the given text in escape codes corresponding to the given parameters.
-- | For example:
-- |
-- | ```purescript
-- | Console.log $ withGraphics (bold <> underline <> foreground BrightRed) "hello world"
-- | ```
-- |
-- | would print "hello world" to the terminal, bold, underlined, and in bright
-- | red, and then reset (so that further logging to the console uses the
-- | normal color and style).
-- |
-- | This function simply wraps the given text in an escape code and a reset
-- | code, so that it is a little more comfortable to use than the functions
-- | in `Ansi.Codes`.
withGraphics :: NonEmptyList GraphicsParam -> String -> String
withGraphics params text =
  escapeCodeToString (Graphics params) <>
  text <>
  escapeCodeToString (Graphics (pure Reset))

bold :: NonEmptyList GraphicsParam
bold = pure (PMode Bold)

dim :: NonEmptyList GraphicsParam
dim = pure (PMode Dim)

italic :: NonEmptyList GraphicsParam
italic = pure (PMode Italic)

underline :: NonEmptyList GraphicsParam
underline = pure (PMode Underline)

inverse :: NonEmptyList GraphicsParam
inverse = pure (PMode Inverse)

strikethrough :: NonEmptyList GraphicsParam
strikethrough = pure (PMode Strikethrough)

foreground :: Color -> NonEmptyList GraphicsParam
foreground c = pure (PForeground c)

background :: Color -> NonEmptyList GraphicsParam
background c = pure (PBackground c)
