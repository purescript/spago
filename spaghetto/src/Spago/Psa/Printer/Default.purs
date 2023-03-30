-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Printer.Default
  ( renderWarning
  , renderError
  , renderStats
  , renderVerboseStats
  , print
  ) where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output (foreground, dim)
import Data.Array as Array
import Data.Foldable (sum, maximum)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.FoldableWithIndex (forWithIndex_)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Foreign.Object as FO
import Spago.Psa.Output (OutputStats, Output)
import Spago.Psa.Printer (Rendered, AnsiText, ansiLength, renderSource, plain, style, indent, line, para, render)
import Spago.Psa.Types (Lines, Position, PsaAnnotedError, PsaOptions, PsaPath(..), StatVerbosity(..))
import Spago.Psa.Util (replicate)

-- | Prints output to the console.
print :: PsaOptions -> Output -> Effect Unit
print options output = do
  forWithIndex_ output.warnings \i warning -> do
    Console.error $ toString (renderWarning lenWarnings (i + 1) warning)
    Console.error ""

  forWithIndex_ output.errors \i error -> do
    Console.error $ toString (renderError lenErrors (i + 1) error)
    Console.error ""

  Console.error $ toString (renderStats' output.stats)

  where
  toString = render options.ansi
  lenWarnings = Array.length output.warnings
  lenErrors = Array.length output.errors
  renderStats' = case options.statVerbosity of
    NoStats -> mempty
    CompactStats -> renderStats
    VerboseStats -> renderVerboseStats

renderWarning :: Int -> Int -> PsaAnnotedError -> Rendered
renderWarning = renderWrapper (foreground Ansi.Yellow)

renderError :: Int -> Int -> PsaAnnotedError -> Rendered
renderError = renderWrapper (foreground Ansi.Red)

renderWrapper :: NonEmptyList Ansi.GraphicsParam -> Int -> Int -> PsaAnnotedError -> Rendered
renderWrapper gfx total index { error, path, position, source, message } =
  para
    [ line $
        [ renderStatus gfx total index error.errorCode
        , plain " "
        , renderPath path error.moduleName
        ] <> fromMaybe mempty (renderPosition <$> position)
    , emptyLine
    , indented
        $ fromMaybe mempty (renderSource' <$> position <*> source)
            <> toLines message
    ]

toLines :: String -> Rendered
toLines = para <<< map (line <<< Array.singleton <<< plain) <<< Str.split (Str.Pattern "\n")

emptyLine :: Rendered
emptyLine = line [ plain "" ]

indented :: Rendered -> Rendered
indented = indent [ plain "  " ]

renderStatus :: NonEmptyList Ansi.GraphicsParam -> Int -> Int -> String -> AnsiText
renderStatus gfx total index code =
  style gfx $ "[" <> show index <> "/" <> show total <> " " <> code <> "]"

renderModuleName :: Maybe String -> AnsiText
renderModuleName Nothing = style dim "(unknown module)"
renderModuleName (Just m) = plain m

renderPath :: PsaPath -> Maybe String -> AnsiText
renderPath (Src f) _ = plain f
renderPath (Lib f) _ = plain f
renderPath _ m = renderModuleName m

renderPosition :: Position -> Array AnsiText
renderPosition pos =
  [ style dim ":"
  , plain (show pos.startLine)
  , style dim ":"
  , plain (show pos.startColumn)
  ]

renderSource' :: Position -> Lines -> Rendered
renderSource' pos lines = renderSource pos lines <> emptyLine

renderStats :: OutputStats -> Rendered
renderStats stats =
  renderStatCols
    [ [ style (foreground Ansi.Yellow) "Warnings" ]
    , [ style (foreground Ansi.Red) "Errors" ]
    ]
    [ renderStat srcWarnings
    , renderStat srcErrors
    ]
    [ renderStat libWarnings
    , renderStat libErrors
    ]
    [ renderStat allWarnings
    , renderStat allErrors
    ]
  where
  sumRatio (Tuple a b) _ (Tuple c d) = Tuple (a + c) (b + d)
  srcWarnings = FO.fold sumRatio (Tuple 0 0) stats.srcWarnings
  srcErrors = FO.fold sumRatio (Tuple 0 0) stats.srcErrors
  libWarnings = FO.fold sumRatio (Tuple 0 0) stats.libWarnings
  libErrors = FO.fold sumRatio (Tuple 0 0) stats.libErrors
  allWarnings = FO.fold sumRatio (Tuple 0 0) stats.allWarnings
  allErrors = FO.fold sumRatio (Tuple 0 0) stats.allErrors

renderVerboseStats :: OutputStats -> Rendered
renderVerboseStats stats =
  renderStatCols
    (warningLabels <> errorLabels)
    (srcWarnings <> srcErrors)
    (libWarnings <> libErrors)
    (allWarnings <> allErrors)
  where
  warnings = Array.sort (FO.keys stats.allWarnings)
  errors = Array.sort (FO.keys stats.allErrors)

  warningLabels = Array.singleton <<< style (foreground Ansi.Yellow) <$> warnings
  errorLabels = Array.singleton <<< style (foreground Ansi.Red) <$> errors

  getStat key x = fromMaybe (Tuple 0 0) $ FO.lookup key x
  getStats ks x = (\k -> renderStat $ getStat k x) <$> ks

  srcWarnings = getStats warnings stats.srcWarnings
  srcErrors = getStats errors stats.srcErrors
  libWarnings = getStats warnings stats.libWarnings
  libErrors = getStats errors stats.libErrors
  allWarnings = getStats warnings stats.allWarnings
  allErrors = getStats errors stats.allErrors

renderStatCols :: Array (Array AnsiText) -> Array (Array AnsiText) -> Array (Array AnsiText) -> Array (Array AnsiText) -> Rendered
renderStatCols col1 col2 col3 col4 = para $
  catRow
    `map` alignLeft ([ [ plain "" ] ] <> col1)
    `Array.zipWith ($)`
      alignLeft ([ [ plain "Src" ] ] <> col2)
    `Array.zipWith ($)`
      alignLeft ([ [ plain "Lib" ] ] <> col3)
    `Array.zipWith ($)`
      alignLeft ([ [ plain "All" ] ] <> col4)

  where
  gutter = [ plain "   " ]
  catRow c1 c2 c3 c4 =
    line $ c1 <> gutter <> c2 <> gutter <> c3 <> gutter <> c4

  lineLength = sum <<< map ansiLength
  alignRight = align \a as -> Array.cons a as
  alignLeft = align \a as -> Array.snoc as a
  align f ls = map pad ls
    where
    max = fromMaybe 0 $ maximum (map lineLength ls)
    pad l = f (plain $ replicate (max - lineLength l) " ") l

renderStat :: Tuple Int Int -> Array AnsiText
renderStat (Tuple 0 0) = [ style (foreground Ansi.Green) "0" ]
renderStat (Tuple a b) | a == b = [ plain $ show a ]
renderStat (Tuple a b) =
  [ plain $ show a
  , style dim $ "/" <> show b
  ]
