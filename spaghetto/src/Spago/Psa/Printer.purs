-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Printer
  ( Row()
  , line
  , indent
  , para
  , AnsiText()
  , plain
  , style
  , ansiLength
  , Rendered
  , render
  , renderRow
  , renderAnsi
  , renderSource
  ) where

import Prelude
import Prim hiding (Row)

import Ansi.Codes as Ansi
import Ansi.Output (foreground, dim)
import Data.Array as Array
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.String as Str
import Data.Tuple (uncurry)
import Spago.Psa.Types (Position, Lines)
import Spago.Psa.Util (replicate, padLeft)

type Rendered = Row (Array AnsiText)

data Row a
  = Line a
  | Indent a (Row a)
  | Para (Array (Row a))

line :: forall a. a -> Row a
line = Line

indent :: forall a. a -> Row a -> Row a
indent = Indent

para :: forall a. Array (Row a) -> Row a
para = Para

instance semigroupRow :: Semigroup (Row a) where
  append a@(Line _) b@(Para bs) = Para (Array.cons a bs)
  append a@(Indent _ _) b@(Para bs) = Para (Array.cons a bs)
  append a@(Para as) b@(Line _) = Para (Array.snoc as b)
  append a@(Para as) b@(Indent _ _) = Para (Array.snoc as b)
  append a@(Para as) b@(Para bs) = Para (as <> bs)
  append a b = Para [ a, b ]

instance monoidRow :: Monoid (Row a) where
  mempty = Para []

data AnsiText
  = Plain String
  | Style (NonEmptyList Ansi.GraphicsParam) String

plain :: String -> AnsiText
plain = Plain

style :: NonEmptyList Ansi.GraphicsParam -> String -> AnsiText
style = Style

ansiLength :: AnsiText -> Int
ansiLength (Plain a) = Str.length a
ansiLength (Style _ a) = Str.length a

render :: Boolean -> Rendered -> String
render ansi = renderRow (Str.joinWith "" <<< map (renderAnsi ansi))

renderRow :: forall a. (a -> String) -> Row a -> String
renderRow f = go ""
  where
  go ind (Line a) = ind <> f a
  go ind (Para as) = Str.joinWith "\n" (go ind <$> as)
  go ind (Indent i as) = go (ind <> f i) as

renderAnsi :: Boolean -> AnsiText -> String
renderAnsi false (Plain s) = s
renderAnsi false (Style _ s) = s
renderAnsi true (Plain s) = s
renderAnsi true (Style g s) =
  Ansi.escapeCodeToString (Ansi.Graphics g) <> s <> Ansi.escapeCodeToString (Ansi.Graphics (NEL.singleton Ansi.Reset))

renderSource :: Position -> Lines -> Rendered
renderSource pos lines = renderAnnotation (gutter + 2) pos source'
  where
  lineNums = Array.range pos.startLine pos.endLine
  gutter = Str.length (show pos.endLine)
  source = uncurry (sourceLine gutter "  ") <$> Array.zip lineNums lines
  source' =
    if Array.length source > 7 then Array.take 3 source
      <> [ [ plain $ replicate (gutter + 2) " ", style dim "..." ] ]
      <> Array.drop (Array.length source - 3) source
    else source

renderAnnotation :: Int -> Position -> Array (Array AnsiText) -> Rendered
renderAnnotation offset pos lines = para
  case lines of
    [ l ] ->
      [ line l
      , line $ renderErrorRange (pos.startColumn + offset) (pos.endColumn - pos.startColumn)
      ]
    _ ->
      [ line $ renderErrorTick (pos.startColumn + offset) "v"
      , para (line <$> lines)
      , line $ renderErrorTick (pos.endColumn + offset - 1) "^"
      ]

sourceLine :: Int -> String -> Int -> String -> Array AnsiText
sourceLine gutter sep num code =
  [ style dim $ padLeft gutter (show num) <> sep
  , plain code
  ]

renderErrorRange :: Int -> Int -> Array AnsiText
renderErrorRange start len =
  [ plain $ replicate (start - 1) " "
  , style (foreground Ansi.Red) $ replicate len "^"
  ]

renderErrorTick :: Int -> String -> Array AnsiText
renderErrorTick start char =
  [ plain $ replicate (start - 1) " "
  , style (foreground Ansi.Red) char
  ]
