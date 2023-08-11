module Dodo.Box
  ( DocBox
  , DocBoxBuffer
  , DocAnnStk
  , Vertical(..)
  , Horizontal(..)
  , Align(..)
  , BoxSize
  , valign
  , halign
  , vappend
  , happend
  , vertical
  , verticalWithAlign
  , horizontal
  , horizontalWithAlign
  , resize
  , fill
  , vpadding
  , hpadding
  , sizeOf
  , isEmpty
  , empty
  , toDoc
  , docBox
  ) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, under)
import Dodo (Doc, Printer(..), annotate)
import Dodo as Dodo
import Dodo.Internal as Internal
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

data Align
  = Start
  | Middle
  | End

derive instance Eq Align

type BoxSize =
  { width :: Int
  , height :: Int
  }

-- | Unlike `Doc`s, which can only be joined along a line, `DocBox`es
-- | are two-dimensional units which can be stacked vertically and
-- | horizontally (eg. for tables).
-- |
-- | `Doc`s can be lifted into `DocBox` by using the `doxBox` printer.
-- | ```purescript
-- | example = Dodo.print docBox twoSpaces myDoc
-- | ````
data DocBox a
  = DocLine (Doc a) Int
  | DocVApp (DocBox a) (DocBox a) BoxSize
  | DocHApp (DocBox a) (DocBox a) BoxSize
  | DocAlign Align Align (DocBox a)
  | DocPad BoxSize
  | DocEmpty

derive instance Functor DocBox

-- | A newtype whose Semigroup instance stacks DocBoxes vertically.
newtype Vertical a = Vertical (DocBox a)

derive instance Newtype (Vertical a) _

derive newtype instance Functor Vertical

instance Semigroup (Vertical a) where
  append = coerce (vappend :: DocBox a -> _ -> _)

instance Monoid (Vertical a) where
  mempty = Vertical DocEmpty

-- | A newtype whose Semigroup instance stacks DocBoxes horizontally.
newtype Horizontal a = Horizontal (DocBox a)

derive instance Newtype (Horizontal a) _

derive newtype instance Functor Horizontal

instance Semigroup (Horizontal a) where
  append = coerce (happend :: DocBox a -> _ -> _)

instance Monoid (Horizontal a) where
  mempty = Horizontal DocEmpty

-- | Joins DocBoxes in a vertical run.
vertical :: forall f a. Foldable f => f (DocBox a) -> DocBox a
vertical = foldr vappend DocEmpty

-- | Joins DocBoxes in a vertical run with uniform horizontal alignment.
verticalWithAlign :: forall f a. Foldable f => Align -> f (DocBox a) -> DocBox a
verticalWithAlign align = foldr (\a b -> halign align a `vappend` b) DocEmpty

-- | Joins DocBoxes in a horizontal run.
horizontal :: forall f a. Foldable f => f (DocBox a) -> DocBox a
horizontal = foldr happend DocEmpty

-- | Joins DocBoxes in a horizontal run with uniform vertical alignment.
horizontalWithAlign :: forall f a. Foldable f => Align -> f (DocBox a) -> DocBox a
horizontalWithAlign align = foldr (\a b -> valign align a `happend` b) DocEmpty

-- | Joins two DocBoxes vertically on top of each other.
vappend :: forall a. DocBox a -> DocBox a -> DocBox a
vappend = case _, _ of
  DocEmpty, b -> b
  a, DocEmpty -> a
  DocPad sizea, DocPad sizeb ->
    DocPad (scale sizea sizeb)
  a, b ->
    DocVApp a b (scale (sizeOf a) (sizeOf b))
  where
  scale sizea sizeb =
    { width: max sizea.width sizeb.width
    , height: sizea.height + sizeb.height
    }

-- | Joins two DocBoxes horizontally next to each other.
happend :: forall a. DocBox a -> DocBox a -> DocBox a
happend = case _, _ of
  DocEmpty, b -> b
  a, DocEmpty -> a
  DocPad sizea, DocPad sizeb ->
    DocPad (scale sizea sizeb)
  a, b -> do
    DocHApp a b (scale (sizeOf a) (sizeOf b))
  where
  scale sizea sizeb =
    { width: sizea.width + sizeb.width
    , height: max sizea.height sizeb.height
    }

-- | Pads a DocBox vertically to fit the tallest box within a
-- | horizontal run.
-- | ```purescript
-- | example =
-- |   valign Middle shortDoc
-- |     `happend` tallDoc
-- | ```
valign :: forall a. Align -> DocBox a -> DocBox a
valign a = case _ of
  DocAlign _ b doc
    | Start <- a, Start <- b -> doc
    | otherwise ->
        DocAlign a b doc
  other ->
    DocAlign a Start other

-- | Pads a DocBox horizontally to fit the widest line within a
-- | vertical run.
-- | ```purescript
-- | example =
-- |   halign Middle skinnyDoc
-- |     `vappend` wideDoc
-- | ```
halign :: forall a. Align -> DocBox a -> DocBox a
halign b = case _ of
  DocAlign a _ doc
    | Start <- a, Start <- b -> doc
    | otherwise ->
        DocAlign a b doc
  other ->
    DocAlign Start b other

-- | Resizes a box to a larger or equivalent size, positioning its content
-- | according to its alignment.
resize :: forall a. BoxSize -> DocBox a -> DocBox a
resize newSize box = vdoc
  where
  box' = case box of
    DocAlign _ _ b -> b
    _ -> box
  size = sizeOf box
  hpad = newSize.width - size.width
  vpad = newSize.height - size.height
  hdoc
    | hpad <= 0 = valign Start box'
    | otherwise = padWithAlign happend hpadding hpad box (halignOf box)
  vdoc
    | vpad <= 0 = halign Start hdoc
    | otherwise = padWithAlign vappend vpadding vpad hdoc (valignOf box)

padWithAlign :: forall a. (a -> a -> a) -> (Int -> a) -> Int -> a -> Align -> a
padWithAlign appendFn paddingFn padWidth doc = case _ of
  Start ->
    doc `appendFn` paddingFn padWidth
  Middle -> do
    let mid = Int.toNumber padWidth / 2.0
    paddingFn (Int.floor mid)
      `appendFn` doc
      `appendFn` paddingFn (Int.ceil mid)
  End ->
    paddingFn padWidth `appendFn` doc

-- | Fills a box to a given size with a Doc. The Doc is assumed
-- | to be 1x1. Providing a Doc of a different size will result
-- | in incorrect layouts.
-- | ```
-- | example =
-- |   fill (Ansi.dim (Dodo.text "-"))
-- |     { width: 100
-- |     , height: 1
-- |     }
-- | ```
fill :: forall a. Doc a -> BoxSize -> DocBox a
fill ch { width, height } = under Vertical (flip power height) line
  where
  line = case ch of
    Internal.Annotate a doc ->
      DocLine (annotate a (power doc width)) width
    _ ->
      DocLine (power ch width) width

-- | Vertical padding of a specific height.
vpadding :: forall a. Int -> DocBox a
vpadding height
  | height <= 0 = DocEmpty
  | otherwise = DocPad { height, width: 0 }

-- | Horizontal padding of a specific width.
hpadding :: forall a. Int -> DocBox a
hpadding width
  | width <= 0 = DocEmpty
  | otherwise = DocPad { height: 1, width }

-- | Returns the size of a DocBox.
sizeOf :: forall a. DocBox a -> BoxSize
sizeOf = case _ of
  DocLine _ width -> { width, height: 1 }
  DocVApp _ _ size -> size
  DocHApp _ _ size -> size
  DocAlign _ _ doc -> sizeOf doc
  DocPad size -> size
  DocEmpty -> { width: 0, height: 0 }

valignOf :: forall a. DocBox a -> Align
valignOf = case _ of
  DocAlign v _ _ -> v
  _ -> Start

halignOf :: forall a. DocBox a -> Align
halignOf = case _ of
  DocAlign _ h _ -> h
  _ -> Start

-- | The identity DocBox.
empty :: forall a. DocBox a
empty = DocEmpty

-- | Checks whether a DocBox is empty.
isEmpty :: forall a. DocBox a -> Boolean
isEmpty = case _ of
  DocEmpty -> true
  _ -> false

-- | Converts a DocBox back into Doc for printing.
toDoc :: forall a. DocBox a -> Doc a
toDoc = go1 <<< resume <<< build AsIs StpDone
  where
  go1 = case _ of
    Nothing -> mempty
    Just { line, next } ->
      go2 (formatLine line) (resume next)

  go2 acc = case _ of
    Nothing -> acc
    Just { line, next } ->
      go2 (acc <> Dodo.break <> formatLine line) (resume next)

formatLine :: forall a. DocLine a -> Doc a
formatLine = go mempty <<< List.singleton
  where
  go acc = case _ of
    List.Nil ->
      acc
    line : lines ->
      case line of
        LinePad w
          | Dodo.isEmpty acc ->
              go acc lines
          | otherwise ->
              go (power Dodo.space w <> acc) lines
        LineDoc doc ->
          go (doc <> acc) lines
        LineAppend a b ->
          go acc (b : a : lines)

data DocBoxStep a
  = StpDone
  | StpLine (Doc a) (DocBoxStep a)
  | StpPad Int Int (DocBoxStep a)
  | StpHorz (DocBoxStep a) (DocBoxStep a) (DocBoxStep a)

data DocBuildSize
  = FullHeight Int
  | FullWidth Int
  | AsIs

data DocBuildCmd a
  = BuildEnter DocBuildSize (DocBoxStep a) (DocBox a)
  | BuildLeave (DocBoxStep a)

data DocBuildStk a
  = BuildVAppR Int (DocBox a) (DocBuildStk a)
  | BuildHAppR Int (DocBox a) (DocBoxStep a) (DocBuildStk a)
  | BuildHAppH (DocBoxStep a) (DocBoxStep a) (DocBuildStk a)
  | BuildNil

build :: forall a. DocBuildSize -> DocBoxStep a -> DocBox a -> DocBoxStep a
build = (\size next box -> go (BuildEnter size next box) BuildNil)
  where
  go cmd stack = case cmd of
    BuildEnter size next box ->
      case size of
        FullHeight height ->
          case box of
            DocHApp a b _ ->
              go (BuildEnter size StpDone b) (BuildHAppR height a next stack)
            _ ->
              go (BuildEnter AsIs next (resize { width: 0, height } box)) stack
        FullWidth width ->
          case box of
            DocVApp a b _ ->
              go (BuildEnter size next b) (BuildVAppR width a stack)
            _ ->
              go (BuildEnter AsIs next (resize { width, height: 0 } box)) stack
        AsIs ->
          case box of
            DocVApp a b { width } ->
              go (BuildEnter (FullWidth width) next b) (BuildVAppR width a stack)
            DocHApp a b { height } ->
              go (BuildEnter (FullHeight height) StpDone b) (BuildHAppR height a next stack)
            DocAlign _ _ a ->
              go (BuildEnter size next a) stack
            DocLine line _ ->
              go (BuildLeave (StpLine line next)) stack
            DocPad padSize ->
              go (BuildLeave (StpPad padSize.width padSize.height next)) stack
            DocEmpty ->
              go (BuildLeave StpDone) stack
    BuildLeave step ->
      case stack of
        BuildVAppR width boxa stk ->
          go (BuildEnter (FullWidth width) step boxa) stk
        BuildHAppR height boxa next stk ->
          go (BuildEnter (FullHeight height) StpDone boxa) (BuildHAppH step next stk)
        BuildHAppH stepb next stk ->
          go (BuildLeave (StpHorz step stepb next)) stk
        BuildNil ->
          step

data DocLine a
  = LinePad Int
  | LineDoc (Doc a)
  | LineAppend (DocLine a) (DocLine a)

type DocProducer a =
  { line :: DocLine a
  , next :: DocBoxStep a
  }

data DocResumeCmd a
  = ResumeEnter (DocBoxStep a)
  | ResumeLeave (Maybe (DocProducer a))

data DocResumeStk a
  = ResumeHorzR (DocBoxStep a) (DocBoxStep a) (DocResumeStk a)
  | ResumeHorzH (Maybe (DocProducer a)) (DocBoxStep a) (DocResumeStk a)
  | ResumeNil

resume :: forall a. DocBoxStep a -> Maybe (DocProducer a)
resume = flip go ResumeNil <<< ResumeEnter
  where
  go cmd stack = case cmd of
    ResumeEnter step ->
      case step of
        StpDone ->
          go (ResumeLeave Nothing) stack
        StpLine doc next -> do
          go (ResumeLeave $ Just { line: LineDoc doc, next }) stack
        StpPad width height next ->
          if height == 0 then
            go (ResumeEnter next) stack
          else
            go
              ( ResumeLeave $ Just
                  { line: LinePad width
                  , next: StpPad width (height - 1) next
                  }
              )
              stack
        StpHorz a b next ->
          go (ResumeEnter b) (ResumeHorzR a next stack)
    ResumeLeave prod ->
      case stack of
        ResumeHorzR stepa next stk ->
          go (ResumeEnter stepa) (ResumeHorzH prod next stk)
        ResumeHorzH prodb next stk ->
          case prod, prodb of
            Just a, Just b ->
              go
                ( ResumeLeave $ Just
                    { line: LineAppend a.line b.line
                    , next: StpHorz a.next b.next next
                    }
                )
                stk
            _, _ ->
              go (ResumeEnter next) stk
        ResumeNil ->
          prod

type DocAnnStk a = List (Either a (Doc a))

newtype DocBoxBuffer a = DocBoxBuffer
  { currentIndent :: Doc a
  , currentLine :: DocAnnStk a
  , currentWidth :: Int
  , lines :: DocBox a
  }

-- | A printer which can lift a Doc into DocBox. It is assumed that
-- | the Doc's annotations respect a distributive law:
-- | ``` purescript
-- | annotate ann (a <> b) = annotate ann a <> annotate ann b
-- | ````
docBox :: forall a. Printer (DocBoxBuffer a) a (DocBox a)
docBox = Printer
  { emptyBuffer
  , writeText
  , writeIndent
  , writeBreak
  , enterAnnotation
  , leaveAnnotation
  , flushBuffer
  }
  where
  emptyBuffer :: DocBoxBuffer a
  emptyBuffer = DocBoxBuffer
    { currentIndent: mempty
    , currentLine: List.Nil
    , currentWidth: 0
    , lines: DocEmpty
    }

  writeText :: Int -> String -> DocBoxBuffer a -> DocBoxBuffer a
  writeText width text (DocBoxBuffer buff) = do
    let
      doc' = Internal.Text width text
      line = case buff.currentLine of
        Right doc : rest ->
          Right (doc <> doc') : rest
        rest ->
          Right doc' : rest
    DocBoxBuffer buff
      { currentLine = line
      , currentWidth = buff.currentWidth + width
      }

  writeIndent :: Int -> String -> DocBoxBuffer a -> DocBoxBuffer a
  writeIndent width text (DocBoxBuffer buff) = do
    let doc = Internal.Text width text
    DocBoxBuffer buff
      { currentIndent = buff.currentIndent <> doc
      , currentWidth = buff.currentWidth + width
      }

  writeBreak :: DocBoxBuffer a -> DocBoxBuffer a
  writeBreak (DocBoxBuffer buff) = do
    let line = stkToDoc buff.currentLine
    DocBoxBuffer buff
      { currentIndent = mempty
      , currentLine = List.filter isLeft buff.currentLine
      , currentWidth = 0
      , lines = buff.lines `vappend` DocLine (buff.currentIndent <> line) buff.currentWidth
      }

  enterAnnotation :: a -> List a -> DocBoxBuffer a -> DocBoxBuffer a
  enterAnnotation ann _ (DocBoxBuffer buff) =
    DocBoxBuffer buff
      { currentLine = Left ann : buff.currentLine
      }

  leaveAnnotation :: a -> List a -> DocBoxBuffer a -> DocBoxBuffer a
  leaveAnnotation _ _ (DocBoxBuffer buff) = do
    let
      line = case buff.currentLine of
        Right doc : Left ann : rest ->
          Right (annotate ann doc) : rest
        Left _ : rest ->
          rest
        _ ->
          unsafeCrashWith "leaveAnnotation: docs and annotations must be interleaved"
    DocBoxBuffer buff
      { currentLine = line
      }

  flushBuffer :: DocBoxBuffer a -> DocBox a
  flushBuffer (DocBoxBuffer buff)
    | isEmpty buff.lines && List.null buff.currentLine =
        DocEmpty
    | otherwise = do
        let line = stkToDoc buff.currentLine
        buff.lines `vappend` DocLine (buff.currentIndent <> line) buff.currentWidth

  stkToDoc :: DocAnnStk a -> Doc a
  stkToDoc = foldl
    ( \doc -> case _ of
        Left ann ->
          annotate ann doc
        Right doc' ->
          doc' <> doc
    )
    mempty
