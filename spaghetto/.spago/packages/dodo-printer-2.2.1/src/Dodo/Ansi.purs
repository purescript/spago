-- | This module provides functions printing with cascading ANSI styles.
-- | ANSI annotations closer to the root will cascade down to child nodes,
-- | where styles closer to the leaves take precedence. Indentation is
-- | never printed with ANSI styles, only the text elements of the document.
module Dodo.Ansi
  ( module Dodo.Ansi
  , module Exports
  ) where

import Prelude

import Ansi.Codes (Color(..), GraphicsParam) as Exports
import Ansi.Codes as Ansi
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Dodo (Doc, Printer(..), annotate)

-- | Resets all cascading styles for a document so that outer styles won't
-- | interfere with inner styles.
reset :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
reset = annotate Ansi.Reset

-- | Prints a document with bold styling.
bold :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
bold = annotate (Ansi.PMode Ansi.Bold)

-- | Prints a document with dim styling.
dim :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
dim = annotate (Ansi.PMode Ansi.Dim)

-- | Prints a document with italic styling.
italic :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
italic = annotate (Ansi.PMode Ansi.Italic)

-- | Prints a document with underline styling.
underline :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
underline = annotate (Ansi.PMode Ansi.Underline)

-- | Prints a document with inverse styling.
inverse :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
inverse = annotate (Ansi.PMode Ansi.Inverse)

-- | Prints a document with strikethrough styling.
strikethrough :: Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
strikethrough = annotate (Ansi.PMode Ansi.Strikethrough)

-- | Prints a document with a specific foreground color.
foreground :: Ansi.Color -> Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
foreground color = annotate (Ansi.PForeground color)

-- | Prints a document with a specific background color.
background :: Ansi.Color -> Doc Ansi.GraphicsParam -> Doc Ansi.GraphicsParam
background color = annotate (Ansi.PBackground color)

newtype AnsiBuffer = AnsiBuffer
  { output :: String
  , pending :: Maybe (NonEmptyList Ansi.GraphicsParam)
  , current :: List Ansi.GraphicsParam
  , previous :: List Ansi.GraphicsParam
  }

ansiGraphics :: Printer AnsiBuffer Ansi.GraphicsParam String
ansiGraphics = Printer
  { emptyBuffer
  , writeText
  , writeIndent
  , writeBreak
  , enterAnnotation
  , leaveAnnotation
  , flushBuffer
  }
  where
  emptyBuffer :: AnsiBuffer
  emptyBuffer =
    AnsiBuffer
      { output: ""
      , pending: Nothing
      , current: List.Nil
      , previous: List.Nil
      }

  resetCode :: String
  resetCode = Ansi.escapeCodeToString $ Ansi.Graphics $ pure Ansi.Reset

  writeText :: Int -> String -> AnsiBuffer -> AnsiBuffer
  writeText _ text output = do
    let AnsiBuffer buffer = writePendingGraphics output
    AnsiBuffer buffer
      { output = buffer.output <> text
      , previous = List.Nil
      }

  writeIndent :: Int -> String -> AnsiBuffer -> AnsiBuffer
  writeIndent _ text (AnsiBuffer buffer) =
    AnsiBuffer buffer
      { output = buffer.output <> text
      }

  writeBreak :: AnsiBuffer -> AnsiBuffer
  writeBreak (AnsiBuffer buffer) = do
    let pending = getPendingGraphics buffer.current
    let resetOrEmpty = if isNothing pending && List.null buffer.previous then "" else resetCode
    AnsiBuffer buffer
      { output = buffer.output <> resetOrEmpty <> "\n"
      , pending = pending
      }

  enterAnnotation :: Ansi.GraphicsParam -> List Ansi.GraphicsParam -> AnsiBuffer -> AnsiBuffer
  enterAnnotation a as (AnsiBuffer buffer) = do
    let current = getCurrentGraphics (List.Cons a as)
    AnsiBuffer buffer
      { pending = getPendingGraphics current
      , current = current
      , previous = buffer.current
      }

  leaveAnnotation :: Ansi.GraphicsParam -> List Ansi.GraphicsParam -> AnsiBuffer -> AnsiBuffer
  leaveAnnotation _ as (AnsiBuffer buffer) = do
    let current = getCurrentGraphics as
    AnsiBuffer buffer
      { pending = Just $ fromMaybe (pure Ansi.Reset) $ getPendingGraphics current
      , current = current
      , previous = buffer.current
      }

  flushBuffer :: AnsiBuffer -> String
  flushBuffer ansiBuffer = do
    let AnsiBuffer buffer = writePendingGraphics ansiBuffer
    buffer.output

  writePendingGraphics :: AnsiBuffer -> AnsiBuffer
  writePendingGraphics (AnsiBuffer buffer) =
    case buffer.pending of
      Nothing ->
        AnsiBuffer buffer
      Just gfx ->
        AnsiBuffer buffer
          { output = buffer.output <> Ansi.escapeCodeToString (Ansi.Graphics gfx)
          , pending = Nothing
          }

  getCurrentGraphics :: List Ansi.GraphicsParam -> List Ansi.GraphicsParam
  getCurrentGraphics =
    List.takeWhile (_ /= Ansi.Reset)
      >>> List.nubByEq graphicsConflict
      >>> List.reverse

  getPendingGraphics :: List Ansi.GraphicsParam -> Maybe (NonEmptyList Ansi.GraphicsParam)
  getPendingGraphics =
    NonEmptyList.fromList
      >>> map (NonEmptyList.cons Ansi.Reset)

  graphicsConflict :: Ansi.GraphicsParam -> Ansi.GraphicsParam -> Boolean
  graphicsConflict = case _, _ of
    Ansi.Reset, Ansi.Reset -> true
    Ansi.PForeground _, Ansi.PForeground _ -> true
    Ansi.PBackground _, Ansi.PBackground _ -> true
    Ansi.PMode a, Ansi.PMode b -> a == b
    _, _ -> false
