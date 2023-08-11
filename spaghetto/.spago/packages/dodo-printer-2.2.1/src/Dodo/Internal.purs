module Dodo.Internal where

import Prelude

import Data.Tuple (Tuple)

-- | Document lines and columns are 0-based offsets.
type Position =
  { line :: Int
  , column :: Int
  , indent :: Int
  , nextIndent :: Int
  , pageWidth :: Int
  , ribbonWidth :: Int
  }

-- | Documents are built using `<>` as horizontal, line-wise concatenation.
-- | The functions in this module let you build documents that respond well
-- | to width constraints (such as `flexGroup` and `flexAlt`).
data Doc a
  = Append (Doc a) (Doc a)
  | Indent (Doc a)
  | Align Int (Doc a)
  | Annotate a (Doc a)
  | FlexSelect (Doc a) (Doc a) (Doc a)
  | FlexAlt (Doc a) (Doc a)
  | WithPosition (Position -> Doc a)
  | Local (LocalOptions -> Tuple LocalOptions (Doc a))
  | Text Int String
  | Break
  | Empty

type LocalOptions =
  { indent :: Int
  , indentSpaces :: String
  , indentUnit :: String
  , indentWidth :: Int
  , pageWidth :: Int
  , ribbonRatio :: Number
  }

derive instance functorDoc :: Functor Doc

instance semigroupDoc :: Semigroup (Doc a) where
  append = bothNotEmpty case _, _ of
    Text n1 str1, Text n2 str2 -> Text (n1 + n2) (str1 <> str2)
    a, b -> Append a b

instance monoidDoc :: Monoid (Doc a) where
  mempty = Empty

-- | Only applies the provided function if both documents are
-- | non-empty, otherwise just yields whichever is non-empty.
bothNotEmpty :: forall a. (Doc a -> Doc a -> Doc a) -> Doc a -> Doc a -> Doc a
bothNotEmpty f = case _, _ of
  Empty, b -> b
  a, Empty -> a
  a, b -> f a b

-- | Only applies the provided function if the document is non-empty.
notEmpty :: forall a. (Doc a -> Doc a) -> Doc a -> Doc a
notEmpty f = case _ of
  Empty -> Empty
  b -> f b

-- | Checks whether the document is empty.
isEmpty :: forall a. Doc a -> Boolean
isEmpty = case _ of
  Empty -> true
  _ -> false
