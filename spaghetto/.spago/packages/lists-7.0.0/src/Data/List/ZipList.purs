-- | This module defines the type of _zip lists_, i.e. linked lists
-- | with a zippy `Applicative` instance.

module Data.List.ZipList
  ( ZipList(..)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable)
import Data.List.Lazy (List, drop, length, repeat, zipWith)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)

-- | `ZipList` is a newtype around `List` which provides a zippy
-- | `Applicative` instance.
newtype ZipList a = ZipList (List a)

instance showZipList :: Show a => Show (ZipList a) where
  show (ZipList xs) = "(ZipList " <> show xs <> ")"

derive instance newtypeZipList :: Newtype (ZipList a) _

derive newtype instance eqZipList :: Eq a => Eq (ZipList a)

derive newtype instance ordZipList :: Ord a => Ord (ZipList a)

derive newtype instance semigroupZipList :: Semigroup (ZipList a)

derive newtype instance monoidZipList :: Monoid (ZipList a)

derive newtype instance foldableZipList :: Foldable ZipList

derive newtype instance traversableZipList :: Traversable ZipList

derive newtype instance functorZipList :: Functor ZipList

instance applyZipList :: Apply ZipList where
  apply (ZipList fs) (ZipList xs) = ZipList (zipWith ($) fs xs)

instance applicativeZipList :: Applicative ZipList where
  pure = ZipList <<< repeat

instance altZipList :: Alt ZipList where
  alt (ZipList xs) (ZipList ys) = ZipList $ xs <> drop (length xs) ys

instance plusZipList :: Plus ZipList where
  empty = mempty

instance alternativeZipList :: Alternative ZipList

instance zipListIsNotBind
  :: Fail (Text """
    ZipList is not Bind. Any implementation would break the associativity law.

    Possible alternatives:
        Data.List.List
        Data.List.Lazy.List
    """)
  => Bind ZipList where
    bind = unsafeCrashWith "bind: unreachable"
