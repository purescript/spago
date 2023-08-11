module Options.Applicative.Help.Chunk
  ( Chunk(..)
  , chunked
  , listToChunk
  , (<<+>>)
  , chunkBeside
  , (<</>>)
  , chunkBesideOrBelow
  , vcatChunks
  , vsepChunks
  , isEmpty
  , stringChunk
  , paragraph
  , extractChunk
  , tabulate
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Data.Foldable (fold, foldr)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..))
import Options.Applicative.Help.Pretty (Doc, fillBreak, indent, text, vcat, (.$.), (<+>), (</>))
import Options.Applicative.Internal.Utils (words)


-- | The free monoid on a semigroup 'a'.
newtype Chunk a = Chunk (Maybe a)
derive instance chunkNewtype :: Newtype (Chunk a) _
derive instance chunkGeneric :: Generic (Chunk a) _
instance chunkShow :: Show a => Show (Chunk a) where show = genericShow
derive newtype instance chunkEq :: Eq a => Eq (Chunk a)
derive newtype instance chunkOrd :: Ord a => Ord (Chunk a)
derive newtype instance chunkFunctor :: Functor Chunk
derive newtype instance chunkApply :: Apply Chunk
derive newtype instance chunkApplicative :: Applicative Chunk
derive newtype instance chunkAlt :: Alt Chunk
derive newtype instance chunkPlus :: Plus Chunk
derive newtype instance chunkAlternative :: Alternative Chunk
derive newtype instance chunkBind :: Bind Chunk
derive newtype instance chunkMonad :: Monad Chunk

instance chunkSemigroup :: Semigroup a => Semigroup (Chunk a) where
  append = chunked append

instance chunkMonoid :: Semigroup a => Monoid (Chunk a) where
  mempty = Chunk Nothing

-- | Given a semigroup structure on 'a', return a monoid structure on 'Chunk a'.
--
-- Note that this is /not/ the same as 'liftA2'.
chunked :: forall a. (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ (Chunk Nothing) y = y
chunked _ x (Chunk Nothing) = x
chunked f (Chunk (Just x)) (Chunk (Just y)) = Chunk (Just (f x y))

-- | Concatenate a list into a Chunk.  'listToChunk' satisfies:
--
-- > isEmpty <<< listToChunk = null
-- > listToChunk = mconcat <<< fmap pure
listToChunk :: forall a. Monoid a => Array a -> Chunk a
listToChunk [] = mempty
listToChunk xs = pure (fold xs)

-- | Part of a constrained comonad instance.
--
-- This is the counit of the adjunction between 'Chunk' and the forgetful
-- functor from monoids to semigroups.  It satisfies:
--
-- > extractChunk <<< pure = id
-- > extractChunk <<< fmap pure = id
extractChunk :: forall a. Monoid a => Chunk a -> a
extractChunk = fromMaybe mempty <<< un Chunk
-- we could also define:
-- duplicate :: Monoid a => Chunk a -> Chunk (Chunk a)
-- duplicate = fmap pure

-- | Concatenate two 'Chunk's with a space in between.  If one is empty, this
-- just returns the other one.
--
-- Unlike '<+>' for 'Doc', this operation has a unit element, namely the empty
-- 'Chunk'.
chunkBeside :: Chunk Doc -> Chunk Doc -> Chunk Doc
chunkBeside = chunked (<+>)
infixr 6 chunkBeside as <<+>>

-- NOTE `<+>` from haskell corresponds to `<+>` (aka `beside`)from ps
-- NOTE `</>` from haskell corresponds to `<+/>` (aka `besideOrBelow`) from ps
-- NOTE `<$>` from haskell corresponds to `</>` (aka `below`) from ps

-- | Concatenate two 'Chunk's with a softline in between.  This is exactly like
-- '<<+>>', but uses a softline instead of a space.
chunkBesideOrBelow :: Chunk Doc -> Chunk Doc -> Chunk Doc
chunkBesideOrBelow = chunked (</>)

infixr 6 chunkBesideOrBelow as <</>>

-- | Concatenate 'Chunk's vertically.
vcatChunks :: Array (Chunk Doc) -> Chunk Doc
vcatChunks = foldr (chunked (.$.)) mempty

-- | Concatenate 'Chunk's vertically separated by empty lines.
vsepChunks :: Array (Chunk Doc) -> Chunk Doc
vsepChunks = foldr (chunked (\x y -> x .$. mempty .$. y)) mempty

-- | Whether a 'Chunk' is empty.  Note that something like 'pure mempty' is not
-- considered an empty chunk, even though the underlying 'Doc' is empty.
isEmpty :: forall a. Chunk a -> Boolean
isEmpty = isNothing <<< un Chunk

-- | Convert a 'String' into a 'Chunk'.  This satisfies:
--
-- > isEmpty <<< stringChunk = null
-- > extractChunk <<< stringChunk = string
stringChunk :: String -> Chunk Doc
stringChunk "" = mempty
stringChunk s = pure (text s)

-- | Convert a paragraph into a 'Chunk'.  The resulting chunk is composed by the
-- words of the original paragraph separated by softlines, so it will be
-- automatically word-wrapped when rendering the underlying document.
--
-- This satisfies:
--
-- > isEmpty <<< paragraph = null <<< words
paragraph :: String -> Chunk Doc
paragraph = foldr (chunked (</>) <<< stringChunk) mempty
          <<< words


tabulate' :: Int -> Array (Tuple Doc Doc) -> Chunk Doc
tabulate' _ [] = mempty
tabulate' size table = pure $ vcat $ table <#> \(Tuple key value) -> indent 2 (fillBreak size key <+> value)

-- | Display pairs of strings in a table.
tabulate :: Array (Tuple Doc Doc) -> Chunk Doc
tabulate = tabulate' 24
