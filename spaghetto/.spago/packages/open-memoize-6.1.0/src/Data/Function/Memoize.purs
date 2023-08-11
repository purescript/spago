-- | This module defines functions for _memoizing_ functions, i.e. creating functions which
-- | remember their results.
-- |
-- | This module works by turning a function into a lazily-evaluated data structure depending on
-- | its domain type.

module Data.Function.Memoize
  ( class Tabulate
  , tabulate
  , memoize
  , memoize2
  , memoize3
  , genericTabulate
  ) where

import Prelude
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Int.Bits ((.&.), zshr)
import Data.Lazy (Lazy, force, defer)
import Data.List (List(..), fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), curry, uncurry)
import Partial.Unsafe (unsafePartial)

-- | The `Tabulate` class identifies those types which can be used as the domain of
-- | a memoized function, i.e. those for which the results can be _tabulated_.
class Tabulate a where
  tabulate :: forall r. (a -> r) -> a -> Lazy r

instance tabulateUnit :: Tabulate Unit where
  tabulate f = let r = defer (\_ -> f unit)
               in \_ -> r

instance tabulateNoArguments :: Tabulate NoArguments where
  tabulate f = let r = defer (\_ -> f NoArguments)
               in \_ -> r

instance tabulateBool :: Tabulate Boolean where
  tabulate f = let r1 = defer (\_ -> f true)
                   r2 = defer (\_ -> f false)
               in \b -> if b then r1 else r2

instance tabulateChar :: Tabulate Char where
  tabulate f = f1 <<< toCharCode
    where
      f1 = tabulate (f <<< unsafePartial fromJust <<< fromCharCode)

instance tabulateString :: Tabulate String where
  tabulate f = f1 <<< toCharArray
    where
      f1 = tabulate (f <<< fromCharArray)

instance tabulateConstructor :: Tabulate a => Tabulate (Constructor name a) where
  tabulate f = let g = tabulate (f <<< Constructor)
               in g <<< \(Constructor a) -> a

instance tabulateArgument :: Tabulate a => Tabulate (Argument a) where
  tabulate f = let g = tabulate (f <<< Argument)
               in g <<< \(Argument a) -> a

instance tabulateMaybe :: Tabulate a => Tabulate (Maybe a) where
  tabulate f = let n = defer (\_ -> f Nothing)
                   j = tabulate (f <<< Just)
               in case _ of
                    Nothing -> n
                    Just a  -> j a

instance tabulateEither :: (Tabulate a, Tabulate b) => Tabulate (Either a b) where
  tabulate f = let l = tabulate (f <<< Left)
                   r = tabulate (f <<< Right)
               in case _ of
                    Left a  -> l a
                    Right b -> r b

instance tabulateSum :: (Tabulate a, Tabulate b) => Tabulate (Sum a b) where
  tabulate f = let l = tabulate (f <<< Inl)
                   r = tabulate (f <<< Inr)
               in case _ of
                    Inl a -> l a
                    Inr b -> r b

instance tabulateTuple :: (Tabulate a, Tabulate b) => Tabulate (Tuple a b) where
  tabulate f = let f' = tabulate \a -> tabulate \b -> f (Tuple a b)
               in \(Tuple a b) -> do g <- f' a
                                     g b

instance tabulateProduct :: (Tabulate a, Tabulate b) => Tabulate (Product a b) where
  tabulate f = let f' = tabulate \a -> tabulate \b -> f (Product a b)
               in \(Product a b) -> do g <- f' a
                                       g b

instance tabulateList :: Tabulate a => Tabulate (List a) where
  tabulate f = let f' = tabulate (f <<< toList)
               in f' <<< fromList
    where
      toList Nothing = Nil
      toList (Just (Tuple head tail)) = Cons head tail

      fromList Nil = Nothing
      fromList (Cons head tail) = Just (Tuple head tail)

instance tabulateArray :: Tabulate a => Tabulate (Array a) where
  tabulate f = f1 <<< fromFoldable
    where
      f1 = tabulate (f <<< toUnfoldable)

data NatTrie a = NatTrie (Lazy a)
                         (Lazy (NatTrie a))
                         (Lazy (NatTrie a))

instance tabulateNat :: Tabulate Int where
  tabulate = tabulateImpl
    where
      tabulateImpl :: forall r. (Int -> r) -> Int -> Lazy r
      tabulateImpl f = go
        where
          go :: Int -> Lazy r
          go n = walk (bits n) trie

          trie :: NatTrie r
          trie = build 0

          build :: Int -> NatTrie r
          build n = NatTrie (defer \_ -> f n)
                            (defer \_ -> build (n * 2))
                            (defer \_ -> build (n * 2 + 1))

          bits :: Int -> List Boolean
          bits = bits' Nil
            where
            bits' acc 0 = acc
            bits' acc n = bits' (Cons (n .&. 1 /= 0) acc) (n `zshr` 1)            

          walk :: forall a. List Boolean -> NatTrie a -> Lazy a
          walk Nil             (NatTrie a _ _) = a
          walk (Cons false bs) (NatTrie _ l _) = l >>= walk bs
          walk (Cons true  bs) (NatTrie _ _ r) = r >>= walk bs

-- | Memoize a function of one argument
memoize :: forall a b. Tabulate a => (a -> b) -> a -> b
memoize f = force <<< f1
  where
    f1 = tabulate f

-- | Memoize a function of two arguments
memoize2 :: forall a b c. Tabulate a => Tabulate b => (a -> b -> c) -> a -> b -> c
memoize2 f = curry f1
  where
    f1 = memoize (uncurry f)

-- | Memoize a function of three arguments
memoize3 :: forall a b c d. Tabulate a => Tabulate b => Tabulate c => (a -> b -> c -> d) -> a -> b -> c -> d
memoize3 f = curry (curry f1)
  where
    f1 = memoize (uncurry (uncurry f))

-- | A default implementation of `Tabulate` for `Generic` types.
-- |
-- | Given a data type made up of data types with `Tabulate` instances:
-- |
-- | ```purescript
-- | data MyDataType
-- |   = A Int
-- |   | B String
-- | ```
-- |
-- | First, derive an instance of `Data.Generics.Rep.Generic`:
-- |
-- | ```purescript
-- | derive instance genericMyDataType :: Generic MyDataType _
-- | ```
-- |
-- | Now, `Tabulate` can be defined in terms of `genericTabulate`:
-- |
-- | ```purescript
-- | instance tabulateMyDataType :: Tabulate MyDataType where
-- |   tabulate = genericTabulate
-- | ```
-- |
-- | _Note_: this function should not be used to derive instances for recursive
-- | data types, and attempting to do so will lead to stack overflow errors
-- | at runtime.
genericTabulate
  :: forall a r rep
   . Generic a rep
  => Tabulate rep
  => (a -> r)
  -> a
  -> Lazy r
genericTabulate f = f1 <<< from
  where
    f1 = tabulate (f <<< to)
