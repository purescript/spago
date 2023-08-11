module Control.Biapply where

import Data.Function (const, identity)

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Tuple (Tuple(..))

-- | A convenience operator which can be used to apply the result of `bipure` in
-- | the style of `Applicative`:
-- |
-- | ```purescript
-- | bipure f g <<$>> x <<*>> y
-- | ```
infixl 4 identity as <<$>>

-- | `Biapply` captures type constructors of two arguments which support lifting of
-- | functions of one or more arguments, in the sense of `Apply`.
class Bifunctor w <= Biapply w where
  biapply :: forall a b c d. w (a -> b) (c -> d) -> w a c -> w b d

infixl 4 biapply as <<*>>

-- | Keep the results of the second computation.
biapplyFirst :: forall w a b c d. Biapply w => w a b -> w c d -> w c d
biapplyFirst a b = bimap (const identity) (const identity) <<$>> a <<*>> b

infixl 4 biapplyFirst as *>>

-- | Keep the results of the first computation.
biapplySecond :: forall w a b c d. Biapply w => w a b -> w c d -> w a b
biapplySecond a b = bimap const const <<$>> a <<*>> b

infixl 4 biapplySecond as <<*

-- | Lift a function of two arguments.
bilift2
  :: forall w a b c d e f
   . Biapply w
  => (a -> b -> c)
  -> (d -> e -> f)
  -> w a d
  -> w b e
  -> w c f
bilift2 f g a b = bimap f g <<$>> a <<*>> b

-- | Lift a function of three arguments.
bilift3
  :: forall w a b c d e f g h
   . Biapply w
  => (a -> b -> c -> d)
  -> (e -> f -> g -> h)
  -> w a e
  -> w b f
  -> w c g
  -> w d h
bilift3 f g a b c = bimap f g <<$>> a <<*>> b <<*>> c

instance biapplyTuple :: Biapply Tuple where
  biapply (Tuple f g) (Tuple a b) = Tuple (f a) (g b)
