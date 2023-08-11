-- | This module defines functions for working with lenses.
module Data.Lens.Lens
  ( lens
  , lens'
  , withLens
  , cloneLens
  , ilens
  , ilens'
  , withIndexedLens
  , cloneIndexedLens
  , lensStore
  , module Data.Lens.Types
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Lens.Internal.Indexed (Indexed(..))
import Data.Lens.Internal.Shop (Shop(..))
import Data.Lens.Types
  ( ALens
  , ALens'
  , AnIndexedLens
  , AnIndexedLens'
  , IndexedLens
  , IndexedLens'
  , Lens
  , Lens'
  )
import Data.Newtype (un)
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))

-- | Create a `Lens` from a getter/setter pair.
-- |
-- | ```purescript
-- | > species = lens _.species $ _ {species = _}
-- | > view species {species : "bovine"}
-- | "bovine"
-- |
-- | > _2 = lens Tuple.snd $ \(Tuple keep _) new -> Tuple keep new
-- | ```
-- |
-- | Note: `_2` is predefined in `Data.Lens.Tuple`.

lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b

lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

withLens :: forall s t a b r. ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens l f = case l (Shop identity \_ b -> b) of Shop x y -> f x y

cloneLens :: forall s t a b. ALens s t a b -> Lens s t a b
cloneLens l = withLens l \x y p -> lens x y p

ilens'
  :: forall i s t a b
   . (s -> Tuple (Tuple i a) (b -> t))
  -> IndexedLens i s t a b
ilens' to pab = dimap to (\(Tuple b f) -> f b) (first ((un Indexed) pab))

-- create an `IndexedLens` from a getter/setter pair.
ilens
  :: forall i s t a b
   . (s -> Tuple i a)
  -> (s -> b -> t)
  -> IndexedLens i s t a b
ilens get set = ilens' \s -> Tuple (get s) \b -> set s b

withIndexedLens
  :: forall i s t a b r
   . (AnIndexedLens i s t a b)
  -> ((s -> (Tuple i a)) -> (s -> b -> t) -> r)
  -> r
withIndexedLens l f = case l (Indexed (Shop identity \_ b -> b)) of Shop x y -> f x y

cloneIndexedLens :: forall i s t a b. AnIndexedLens i s t a b -> IndexedLens i s t a b
cloneIndexedLens l = withIndexedLens l \x y p -> ilens x y p

-- | Converts a lens into the form that `lens'` accepts.
-- |
-- | Can be useful when defining a lens where the focus appears under multiple
-- | constructors of an algebraic data type.  This function would be called for
-- | each case of the data type.
-- |
-- | For example:
-- |
-- | ```
-- | data LensStoreExample = LensStoreA Int | LensStoreB (Tuple Boolean Int)
-- |
-- | lensStoreExampleInt :: Lens' LensStoreExample Int
-- | lensStoreExampleInt = lens' case _ of
-- |   LensStoreA i -> map LensStoreA <$> lensStore identity i
-- |   LensStoreB i -> map LensStoreB <$> lensStore _2 i
-- | ```
lensStore :: forall s t a b. ALens s t a b -> s -> Tuple a (b -> t)
lensStore l = withLens l (lift2 Tuple)

