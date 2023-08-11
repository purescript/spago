module PureScript.CST.Range.TokenList
  ( TokenList
  , singleton
  , cons
  , wrap
  , head
  , UnconsToken(..)
  , uncons
  , uncons'
  , toUnfoldable
  , toArray
  , fromArray
  ) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (SourceToken)

data TokenList
  = TokenEmpty
  | TokenCons SourceToken TokenList
  | TokenWrap SourceToken TokenList SourceToken
  | TokenAppend TokenList TokenList
  | TokenDefer (Unit -> TokenList)
  | TokenArray Int Int (Array SourceToken)

instance lazyTokenList :: Lazy TokenList where
  defer = TokenDefer

instance semigroupTokenList :: Semigroup TokenList where
  append = case _, _ of
    a, TokenEmpty -> a
    TokenEmpty, b -> b
    a, b -> TokenAppend a b

instance monoidTokenList :: Monoid TokenList where
  mempty = TokenEmpty

fromArray :: Array SourceToken -> TokenList
fromArray arr = if len == 0 then TokenEmpty else TokenArray 0 (len - 1) arr
  where
  len = Array.length arr

singleton :: SourceToken -> TokenList
singleton a = TokenCons a TokenEmpty

cons :: SourceToken -> TokenList -> TokenList
cons = TokenCons

wrap :: SourceToken -> TokenList -> SourceToken -> TokenList
wrap = TokenWrap

head :: TokenList -> Maybe SourceToken
head = case _ of
  TokenEmpty -> Nothing
  TokenCons a _ -> Just a
  TokenDefer k -> head (k unit)
  TokenWrap a _ _ -> Just a
  TokenAppend l _ -> head l
  TokenArray ix _ arr -> Just (unsafePartial (unsafeIndex arr ix))

data UnconsToken
  = UnconsDone
  | UnconsMore SourceToken TokenList

uncons :: TokenList -> UnconsToken
uncons = uncons' UnconsDone UnconsMore

uncons' :: forall r. r -> (SourceToken -> TokenList -> r) -> TokenList -> r
uncons' done more = case _ of
  TokenEmpty -> done
  TokenCons a b -> more a b
  TokenWrap a b c -> more a (b <> singleton c)
  TokenAppend a b -> uncons2 done more a b
  TokenDefer k -> uncons' done more (k unit)
  TokenArray ix1 ix2 arr -> do
    let
      next
        | ix1 == ix2 = TokenEmpty
        | otherwise = TokenArray (ix1 + 1) ix2 arr
    more (unsafePartial (unsafeIndex arr ix1)) next

uncons2 :: forall r. r -> (SourceToken -> TokenList -> r) -> TokenList -> TokenList -> r
uncons2 done more l r = case l of
  TokenEmpty -> uncons' done more r
  TokenCons a b -> more a (b <> r)
  TokenWrap a b c -> more a (b <> TokenCons c r)
  TokenAppend a b -> uncons2 done more a (b <> r)
  TokenDefer k -> uncons2 done more (k unit) r
  TokenArray ix1 ix2 arr -> do
    let
      next
        | ix1 == ix2 = r
        | otherwise = TokenArray (ix1 + 1) ix2 arr <> r
    more (unsafePartial (unsafeIndex arr ix1)) next

toUnfoldable :: forall f. Unfoldable f => TokenList -> f SourceToken
toUnfoldable = unfoldr (uncons' Nothing (\a b -> Just (Tuple a b)))

toArray :: TokenList -> Array SourceToken
toArray init = ST.run do
  arr <- STArray.new
  cur <- STRef.new init
  continue <- STRef.new true
  ST.while (STRef.read continue) do
    tree <- STRef.read cur
    case uncons tree of
      UnconsDone -> do
        _ <- STRef.write false continue
        pure unit
      UnconsMore a next -> do
        _ <- STRef.write next cur
        _ <- STArray.push a arr
        pure unit
  STArray.unsafeFreeze arr
