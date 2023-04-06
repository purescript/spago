-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Printer
  ( renderWarning
  , renderError
  , renderStats
  , renderVerboseStats
  , print
  ) where

import Prelude

import Ansi.Codes as Ansi
import Control.Alternative as Alternative
import Data.Array as Array
import Data.Foldable (fold, foldMap, maximum, maximumBy)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.String as Str
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (unfoldr)
import Dodo as D
import Dodo.Ansi as DA
import Effect (Effect)
import Effect.Console as Console
import Foreign.Object as FO
import Spago.Core.Config as Core
import Spago.Psa.Output (OutputStats, Output)
import Spago.Psa.Types (Lines, Position, PsaAnnotedError, PsaOutputOptions, PsaPath(..))

-- | Prints output to the console.
print :: PsaOutputOptions -> Output -> Effect Unit
print options output = do
  forWithIndex_ output.warnings \i warning -> do
    Console.error $ printDoc (renderWarning lenWarnings (i + 1) warning)
    Console.error ""

  forWithIndex_ output.errors \i error -> do
    Console.error $ printDoc (renderError lenErrors (i + 1) error)
    Console.error ""

  Console.error $ printDoc (renderStats' output.stats)

  where
  printDoc
    | options.color = D.print DA.ansiGraphics D.twoSpaces
    | otherwise = D.print D.plainText D.twoSpaces
  lenWarnings = Array.length output.warnings
  lenErrors = Array.length output.errors
  renderStats' = case options.statVerbosity of
    Core.NoStats -> mempty
    Core.CompactStats -> renderStats
    Core.VerboseStats -> renderVerboseStats

renderWarning :: Int -> Int -> PsaAnnotedError -> D.Doc Ansi.GraphicsParam
renderWarning = renderWrapper Ansi.BrightYellow

renderError :: Int -> Int -> PsaAnnotedError -> D.Doc Ansi.GraphicsParam
renderError = renderWrapper Ansi.BrightRed

renderWrapper :: Ansi.Color -> Int -> Int -> PsaAnnotedError -> D.Doc Ansi.GraphicsParam
renderWrapper color total index { error, path, position, source, message } =
  D.foldWithSeparator (D.break <> D.break)
    [ D.words $
        [ renderStatus color total index error.errorCode
        , renderPath path error.moduleName <> foldMap renderPosition position
        ]
    , D.indent $ D.lines
        [ fromMaybe mempty (renderSource' <$> position <*> source)
        , toLines message
        ]
    ]

toLines :: String -> D.Doc Ansi.GraphicsParam
toLines = D.lines <<< map D.text <<< Str.split (Str.Pattern "\n")

renderStatus :: Ansi.Color -> Int -> Int -> String -> D.Doc Ansi.GraphicsParam
renderStatus color total index code =
  DA.foreground color $ D.text $ "[" <> show index <> "/" <> show total <> " " <> code <> "]"

renderModuleName :: Maybe String -> D.Doc Ansi.GraphicsParam
renderModuleName Nothing = DA.dim $ D.text "(unknown module)"
renderModuleName (Just m) = D.text m

renderPath :: PsaPath -> Maybe String -> D.Doc Ansi.GraphicsParam
renderPath (Src f) _ = D.text f
renderPath (Lib f) _ = D.text f
renderPath _ m = renderModuleName m

renderPosition :: Position -> D.Doc Ansi.GraphicsParam
renderPosition pos = fold
  [ DA.dim $ D.text ":"
  , D.text $ show pos.startLine
  , DA.dim $ D.text ":"
  , D.text $ show pos.startColumn
  ]

renderSource' :: Position -> Lines -> D.Doc Ansi.GraphicsParam
renderSource' pos lines = renderSource pos lines <> D.break

renderStats :: OutputStats -> D.Doc Ansi.GraphicsParam
renderStats stats =
  renderStatCols
    { col1: [ renderLabel Ansi.BrightYellow "Warnings", renderLabel Ansi.BrightRed "Errors" ]
    , col2: [ renderStat srcWarnings, renderStat srcErrors ]
    , col3: [ renderStat libWarnings, renderStat libErrors ]
    , col4: [ renderStat allWarnings, renderStat allErrors ]
    }
  where
  renderLabel color lbl = { width: Str.length lbl, alignLeft: true, doc: DA.foreground color $ D.text lbl }
  sumRatio (Tuple a b) _ (Tuple c d) = Tuple (a + c) (b + d)
  srcWarnings = FO.fold sumRatio (Tuple 0 0) stats.srcWarnings
  srcErrors = FO.fold sumRatio (Tuple 0 0) stats.srcErrors
  libWarnings = FO.fold sumRatio (Tuple 0 0) stats.libWarnings
  libErrors = FO.fold sumRatio (Tuple 0 0) stats.libErrors
  allWarnings = FO.fold sumRatio (Tuple 0 0) stats.allWarnings
  allErrors = FO.fold sumRatio (Tuple 0 0) stats.allErrors

renderVerboseStats :: OutputStats -> D.Doc Ansi.GraphicsParam
renderVerboseStats stats =
  renderStatCols
    { col1: warningLabels <> errorLabels
    , col2: srcWarnings <> srcErrors
    , col3: libWarnings <> libErrors
    , col4: allWarnings <> allErrors
    }
  where
  warnings = Array.sort (FO.keys stats.allWarnings)
  errors = Array.sort (FO.keys stats.allErrors)

  renderLabel color lbl =
    { width: Str.length lbl
    , alignLeft: true
    , doc: DA.foreground color $ D.text lbl
    }

  warningLabels = renderLabel Ansi.BrightYellow <$> warnings
  errorLabels = renderLabel Ansi.BrightRed <$> errors

  getStat key x = fromMaybe (Tuple 0 0) $ FO.lookup key x
  getStats ks x = (\k -> renderStat $ getStat k x) <$> ks

  srcWarnings = getStats warnings stats.srcWarnings
  srcErrors = getStats errors stats.srcErrors
  libWarnings = getStats warnings stats.libWarnings
  libErrors = getStats errors stats.libErrors
  allWarnings = getStats warnings stats.allWarnings
  allErrors = getStats errors stats.allErrors

renderStatCols
  :: { col1 :: Array DocColumn
     , col2 :: Array DocColumn
     , col3 :: Array DocColumn
     , col4 :: Array DocColumn
     }
  -> D.Doc Ansi.GraphicsParam
renderStatCols columns =
  renderStatCols' $ columns
    { col1 = Array.cons { width: 0, alignLeft: true, doc: mempty } columns.col1
    , col2 = Array.cons { width: 3, alignLeft: true, doc: D.text "Src" } columns.col2
    , col3 = Array.cons { width: 3, alignLeft: true, doc: D.text "Lib" } columns.col3
    , col4 = Array.cons { width: 3, alignLeft: true, doc: D.text "All" } columns.col4
    }

type DocColumn =
  { width :: Int
  , alignLeft :: Boolean
  , doc :: D.Doc Ansi.GraphicsParam
  }

renderStatCols'
  :: { col1 :: Array DocColumn
     , col2 :: Array DocColumn
     , col3 :: Array DocColumn
     , col4 :: Array DocColumn
     }
  -> D.Doc Ansi.GraphicsParam
renderStatCols' { col1, col2, col3, col4 } = D.lines rows
  where
  maxColWidth :: Array DocColumn -> Int
  maxColWidth = maybe 0 _.width <<< maximumBy (comparing _.width)
  col1Width = maxColWidth col1
  col2Width = maxColWidth col2
  col3Width = maxColWidth col3
  col4Width = maxColWidth col4

  numOfRows = fromMaybe 0 $ maximum $ map Array.length [ col1, col2, col3, col4 ]
  guttered = D.foldWithSeparator (D.text "   ")

  rows :: Array (D.Doc Ansi.GraphicsParam)
  rows = flip unfoldr 0 buildRow

  buildColumn :: Array DocColumn -> Int -> Int -> Maybe (D.Doc Ansi.GraphicsParam)
  buildColumn column colWidth rowIdx = do
    { width, alignLeft, doc } <- Array.index column rowIdx
    let
      padding = colWidth - width
      padText = D.text $ power " " padding
    if padding == 0 then
      pure doc
    else if alignLeft then
      pure $ doc <> padText
    else
      pure $ padText <> doc

  buildRow rowIdx = do
    Alternative.guard (rowIdx /= numOfRows)
    c1 <- buildColumn col1 col1Width rowIdx
    c2 <- buildColumn col2 col2Width rowIdx
    c3 <- buildColumn col3 col3Width rowIdx
    c4 <- buildColumn col4 col4Width rowIdx
    pure $ flip Tuple (rowIdx + 1) $ guttered [ c1, c2, c3, c4 ]

renderStat :: Tuple Int Int -> DocColumn
renderStat (Tuple 0 0) = { width: 1, alignLeft: false, doc: DA.foreground Ansi.BrightGreen $ D.text "0" }
renderStat (Tuple a b)
  | a == b = do
      let aText = show a
      { width: Str.length aText, alignLeft: false, doc: D.text aText }
  | otherwise = do
      let aText = show a
      let bText = show b
      let width = 1 + Str.length aText + Str.length bText
      { width, alignLeft: false, doc: fold [ D.text aText, DA.dim $ D.text "/", D.text bText ] }

renderSource :: Position -> Lines -> D.Doc Ansi.GraphicsParam
renderSource pos lines = renderAnnotation (gutter + 2) pos source'
  where
  lineNums = Array.range pos.startLine pos.endLine
  gutter = Str.length (show pos.endLine)
  source = uncurry (sourceLine gutter "  ") <$> Array.zip lineNums lines
  source' =
    if Array.length source > 7 then Array.take 3 source
      <> [ (D.text $ power " " (gutter + 2)) <> (DA.dim $ D.text "...") ]
      <> Array.drop (Array.length source - 3) source
    else source

renderAnnotation :: Int -> Position -> Array (D.Doc Ansi.GraphicsParam) -> D.Doc Ansi.GraphicsParam
renderAnnotation offset pos lines =
  D.lines case lines of
    [ l ] ->
      [ l
      , renderErrorRange (pos.startColumn + offset) (pos.endColumn - pos.startColumn)
      ]
    _ ->
      [ renderErrorTick (pos.startColumn + offset) "v"
      , D.lines lines
      , renderErrorTick (pos.endColumn + offset - 1) "^"
      ]

sourceLine :: Int -> String -> Int -> String -> D.Doc Ansi.GraphicsParam
sourceLine gutter sep num code = fold
  [ DA.dim $ D.text $ padLeft gutter (show num) <> sep
  , D.text code
  ]

renderErrorRange :: Int -> Int -> D.Doc Ansi.GraphicsParam
renderErrorRange start len = fold
  [ D.text $ power " " (start - 1)
  , DA.foreground Ansi.BrightRed $ D.text $ power "^" len
  ]

renderErrorTick :: Int -> String -> D.Doc Ansi.GraphicsParam
renderErrorTick start char = fold
  [ D.text $ power " " (start - 1)
  , DA.foreground Ansi.BrightRed $ D.text char
  ]

padLeft :: Int -> String -> String
padLeft width str = power " " (width - Str.length str) <> str
