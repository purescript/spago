module Data.Codec where

import Prelude hiding (compose, identity)

import Control.Category as Category
import Data.Bifunctor (lmap)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Tuple (Tuple(..), fst)

data Codec m a b c d = Codec (a → m d) (c → Tuple b d)

instance Functor m ⇒ Functor (Codec m a b c) where
  map f (Codec g h) = Codec (map f <<< g) (map f <<< h)

instance Functor m ⇒ Invariant (Codec m a b c) where
  imap = imapF

instance (Apply m, Semigroup b) ⇒ Apply (Codec m a b c) where
  apply (Codec f g) (Codec h i) = Codec (\a → f a <*> h a) (\c → g c <*> i c)

instance (Applicative m, Monoid b) ⇒ Applicative (Codec m a b c) where
  pure x = Codec (const (pure x)) (const (pure x))

instance Functor m ⇒ Profunctor (Codec m a b) where
  dimap f g (Codec h i) = Codec (map g <<< h) (map g <<< i <<< f)

codec ∷ ∀ m a b c. (a → m c) → (c → b) → Codec m a b c c
codec f g = Codec f (\b → Tuple (g b) b)

type Codec' m a b = Codec m a a b b

codec' ∷ ∀ m a b. (a → m b) → (b → a) → Codec' m a b
codec' f g = Codec f (\b → Tuple (g b) b)

decode ∷ ∀ m a b c d. Codec m a b c d → a → m d
decode (Codec f _) = f

encode ∷ ∀ m a b c d. Codec m a b c d → c → b
encode (Codec _ f) = fst <<< f

hoist ∷ ∀ m m' a b c d. (m ~> m') → Codec m a b c d → Codec m' a b c d
hoist f (Codec g h) = Codec (f <<< g) h

identity ∷ ∀ m a. Applicative m ⇒ Codec m a a a a
identity = codec pure Category.identity

compose ∷ ∀ a d f b e c m. Bind m ⇒ Codec m d c e f → Codec m a b c d → Codec m a b e f
compose (Codec f g) (Codec h i) = Codec (f <=< h) (lmap (fst <<< i) <<< g)

infixr 8 compose as <~<

composeFlipped ∷ ∀ a d f b e c m. Bind m ⇒ Codec m a b c d → Codec m d c e f → Codec m a b e f
composeFlipped = flip compose

infixr 8 composeFlipped as >~>

-- | `Codec` is defined as a `Profunctor` so that `lcmap` can be used to target
-- | specific fields when defining a codec for a product type. This operator
-- | is a convenience for that:
-- |
-- | ``` purescript
-- | tupleCodec =
-- |   Tuple
-- |     <$> fst ~ fstCodec
-- |     <*> snd ~ sndCodec
-- | ```
infixl 5 lcmap as ~
