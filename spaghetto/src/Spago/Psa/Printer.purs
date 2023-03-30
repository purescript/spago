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
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.String as Str
import Data.Tuple (uncurry)
import Dodo as D
import Dodo.Ansi as DA
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

renderSource :: Position -> Lines -> D.Doc Ansi.GraphicsParam
renderSource pos lines = renderAnnotation (gutter + 2) pos source'
  where
  lineNums = Array.range pos.startLine pos.endLine
  gutter = Str.length (show pos.endLine)
  source = uncurry (sourceLine gutter "  ") <$> Array.zip lineNums lines
  source' =
    if Array.length source > 7 then Array.take 3 source
      <> [ (D.text $ replicate (gutter + 2) " ") <> (DA.dim $ D.text "..." ) ]
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
  [ D.text $ replicate (start - 1) " "
  , DA.foreground Ansi.Red $ D.text $ replicate len "^"
  ]

renderErrorTick :: Int -> String -> D.Doc Ansi.GraphicsParam
renderErrorTick start char = fold
  [ D.text $ replicate (start - 1) " "
  , DA.foreground Ansi.Red $ D.text char
  ]
