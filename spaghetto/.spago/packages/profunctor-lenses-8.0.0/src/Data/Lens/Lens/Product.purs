module Data.Lens.Lens.Product where

import Prelude

import Data.Functor.Product (Product)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Lens (Lens)
import Data.Lens.Lens.Tuple as T

-- | Lens for the first component of a `Product`.
_1 :: forall f g h a. Lens (Product f g a) (Product h g a) (f a) (h a)
_1 = _Newtype <<< T._1

-- | Lens for the second component of a `Product`.
_2 :: forall f g h a. Lens (Product f g a) (Product f h a) (g a) (h a)
_2 = _Newtype <<< T._2
