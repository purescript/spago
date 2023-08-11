-- | These combinators will produce `Array`s, as opposed to the other combinators
-- | of the same names in the __Parsing.Combinators__ module
-- | which mostly produce `List`s. These `Array` combinators will run in a bit
-- | less time (*~85% runtime*) than the similar `List` combinators, and they will run in a
-- | lot less time (*~10% runtime*) than the similar combinators in __Data.Array__.
-- |
-- | If there is some other combinator which returns
-- | a `List` but we want an `Array`, and there is no `Array` version of the
-- | combinator in this module, then we can rely on the
-- | [__`Data.Array.fromFoldable`__](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:fromFoldable)
-- | function for a pretty fast transformation from `List` to `Array`.
module Parsing.Combinators.Array
  ( many
  , many1
  , manyTill_
  , manyIndex
  ) where

import Prelude

import Control.Alt (alt)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Parsing (ParseError(..), ParserT, fail, parseErrorMessage, parseErrorPosition)
import Parsing.Combinators (try)

-- | Match the phrase `p` as many times as possible.
-- |
-- | If `p` never consumes input when it
-- | fails then `many p` will always succeed,
-- | but may return an empty array.
many :: forall s m a. ParserT s m a -> ParserT s m (Array a)
many p = do
  rlist <- flip tailRecM Nil $ \xs -> alt
    do
      x <- try p
      pure (Loop (x : xs))
    do
      pure (Done xs)
  pure $ Array.reverse $ Array.fromFoldable rlist

-- | Match the phrase `p` as many times as possible, at least once.
many1 :: forall s m a. ParserT s m a -> ParserT s m (NonEmptyArray a)
many1 p = do
  xs <- many p
  case Array.NonEmpty.fromArray xs of
    Nothing -> fail "Expected at least 1"
    Just xs' -> pure xs'

-- | Parse many phrases until the terminator phrase matches.
-- | Returns the list of phrases and the terminator phrase.
manyTill_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (Array a) e)
manyTill_ p end = do
  Tuple rlist e <- flip tailRecM Nil \xs -> alt
    do
      t <- end
      pure (Done (Tuple xs t))
    do
      x <- p
      pure (Loop (x : xs))
  pure $ Tuple (Array.reverse $ Array.fromFoldable rlist) e

-- | Parse the phrase as many times as possible, at least *N* times, but no
-- | more than *M* times.
-- | If the phrase can’t parse as least *N* times then the whole
-- | parser fails. If the phrase parses successfully *M* times then stop.
-- | The current phrase index, starting at *0*, is passed to the phrase.
-- |
-- | Returns the array of parse results and the number of results.
-- |
-- | `manyIndex n n (\_ -> p)` is equivalent to `replicateA n p`.
manyIndex :: forall s m a. Int -> Int -> (Int -> ParserT s m a) -> ParserT s m (Tuple Int (Array a))
manyIndex from to p =
  if from > to || from < 0 then
    pure (Tuple 0 [])
  else do
    Tuple n rlist <- tailRecM go (Tuple 0 Nil)
    pure $ Tuple n $ Array.reverse $ Array.fromFoldable rlist
  where
  go (Tuple i xs) =
    if i >= to then
      pure (Done (Tuple i xs))
    else catchError
      do
        x <- p i
        pure (Loop (Tuple (i + 1) (x : xs)))
      \e -> do
        if i >= from then
          pure (Done (Tuple i xs))
        else
          throwError $ ParseError
            (parseErrorMessage e <> " (at least " <> show from <> ", but only parsed " <> show i <> ")")
            (parseErrorPosition e)
