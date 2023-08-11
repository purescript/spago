module Data.Monoid.Alternate where

import Prelude

import Control.Alternative (class Alt, class Plus, class Alternative, empty, (<|>))
import Control.Comonad (class Comonad, class Extend)
import Data.Eq (class Eq1)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)

-- | Monoid and semigroup instances corresponding to `Plus` and `Alt` instances
-- | for `f`
-- |
-- | ``` purescript
-- | Alternate fx <> Alternate fy == Alternate (fx <|> fy)
-- | mempty :: Alternate _ == Alternate empty
-- | ```
newtype Alternate :: forall k. (k -> Type) -> k -> Type
newtype Alternate f a = Alternate (f a)

derive instance newtypeAlternate :: Newtype (Alternate f a) _

derive newtype instance eqAlternate :: Eq (f a) => Eq (Alternate f a)

derive newtype instance eq1Alternate :: Eq1 f => Eq1 (Alternate f)

derive newtype instance ordAlternate :: Ord (f a) => Ord (Alternate f a)

derive newtype instance ord1Alternate :: Ord1 f => Ord1 (Alternate f)

derive newtype instance boundedAlternate :: Bounded (f a) => Bounded (Alternate f a)

derive newtype instance functorAlternate :: Functor f => Functor (Alternate f)

derive newtype instance applyAlternate :: Apply f => Apply (Alternate f)

derive newtype instance applicativeAlternate :: Applicative f => Applicative (Alternate f)

derive newtype instance altAlternate :: Alt f => Alt (Alternate f)

derive newtype instance plusAlternate :: Plus f => Plus (Alternate f)

derive newtype instance alternativeAlternate :: Alternative f => Alternative (Alternate f)

derive newtype instance bindAlternate :: Bind f => Bind (Alternate f)

derive newtype instance monadAlternate :: Monad f => Monad (Alternate f)

derive newtype instance extendAlternate :: Extend f => Extend (Alternate f)

derive newtype instance comonadAlternate :: Comonad f => Comonad (Alternate f)

instance showAlternate :: Show (f a) => Show (Alternate f a) where
  show (Alternate a) = "(Alternate " <> show a <> ")"

instance semigroupAlternate :: Alt f => Semigroup (Alternate f a) where
  append (Alternate a) (Alternate b) = Alternate (a <|> b)

instance monoidAlternate :: Plus f => Monoid (Alternate f a) where
  mempty = Alternate empty
