-- | Prisms are used for selecting cases of a type, most often a sum
-- | type. Consider this:
-- |
-- | ```purescript
-- | data Fill -- think of a paint program filling a shape
-- |   = NoFill
-- |   | Solid Color
-- |   | ...
-- | ```
-- |
-- | A prism that focuses on `Solid` fills could be written like this:
-- |
-- | ```purescript
-- | solidFocus :: Prism' Fill Color
-- | solidFocus = prism' Solid case _ of
-- |   Solid color -> Just color
-- |   _ -> Nothing
-- | ```
-- |
-- | ... and used like this:
-- |
-- | ```purescript
-- | preview solidFocus (Solid Color.white) == Just Color.white
-- | preview solidFocus NoFill == Nothing
-- |
-- | is solidFocus (Solid Color.white) == true
-- | ```
-- |
-- | `review` can be used to go from a `Color` to a `Fill`:
-- |
-- | ```purescript
-- | review solidFocus Color.white == Solid Color.white
-- | ```
-- | 
-- | For more information, see `PrismsForSumTypes.purs` in the
-- | `examples/src` directory.
-- |
-- | ---------------
-- |
-- | A well-behaved `Prism` will follow these laws:
-- |
-- | **review-preview**: `preview` retrieves what `review` creates. Equationally:
-- |   
-- | ```purescript
-- | review prism >>> preview prism ≡ Just
-- | ```
-- |
-- | An example:
-- | 
-- | ```purescript
-- | Color.white # review solidFocus # preview solidFocus
-- |   == Just Color.white
-- | ```
-- | 
-- | **preview-review**: If `preview` retrieves something, `review` can create
-- | the original from that something. Equationally:
-- | 
-- | ```purescript
-- | if preview prism s ≡ Just a then review prism a ≡ s
-- | ```
-- |
-- | An example:
-- |
-- | ```purescript
-- | Solid Color.white # preview solidFocus <#> review solidFocus
-- |   == Solid Color.white
-- | ```
module Data.Lens.Prism
  ( prism'
  , prism
  , only
  , nearly
  , review
  , is
  , isn't
  , matching
  , clonePrism
  , withPrism
  , below
  , module ExportTypes
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Either (Either(..), either)
import Data.HeytingAlgebra (ff, tt)
import Data.Lens.Types (APrism, APrism', Market(..), Prism, Prism', Review, Tagged(..))
import Data.Lens.Types (APrism, APrism', Prism, Prism', Review, Review') as ExportTypes
import Data.Maybe (Maybe, maybe)
import Data.Newtype (under)
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice (right)
import Data.Traversable (class Traversable, traverse)

-- | Create a `Prism` from a constructor and a matcher function that
-- | produces an `Either`:
-- | 
-- | ```purescript
-- | solidFocus :: Prism' Fill Color
-- | solidFocus = prism Solid case _ of
-- |   Solid color -> Right color
-- |   anotherCase -> Left anotherCase
-- | ```
-- |
-- | _Note_: The matcher function returns a result wrapped in `Either t`
-- | to allow for type-changing prisms in the case where the input does
-- | not match.
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to fro pab = dimap fro (either identity identity) (right (rmap to pab))

-- | Create a `Prism` from a constructor and a matcher function that
-- | produces a `Maybe`:
-- | 
-- | ```purescript
-- | solidFocus :: Prism' Fill Color
-- | solidFocus = prism' Solid case _ of
-- |   Solid color -> Just color
-- |   _ -> Nothing
-- | ```
prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' to fro = prism to (\s -> maybe (Left s) Right (fro s))

-- | `nearly` is a variant of `only`. Like `only`, `nearly` produces
-- | a prism that matches
-- | a single value. Unlike `only`, it uses a predicate you supply
-- | instead of depending on `class Eq`: 
-- | 
-- | ```purescript
-- | solidWhiteFocus :: Prism' Fill Unit
-- | solidWhiteFocus = nearly (Solid Color.white) predicate
-- |   where
-- |     predicate candidate =
-- |       color.toHexString == Color.white.toHexString
-- | ```
nearly :: forall a. a -> (a -> Boolean) -> Prism' a Unit
nearly x f = prism' (const x) (guard <<< f)

-- | `only` focuses not just on a case, but a specific value of that case.
-- | 
-- | ```purescript
-- | solidWhiteFocus :: Prism' Fill Unit
-- | solidWhiteFocus = only $ Solid Color.white
-- |
-- | is      solidWhiteFocus (Solid Color.white) == true
-- | preview solidWhiteFocus (Solid Color.white) == Just unit
-- | review  solidWhiteFocus unit                == Solid Color.white
-- | ```
-- |
-- | *Note*: `only` depends on `Eq`. Strange definitions of `(==)`
-- | (for example, that it counts any `Fill` as being equal to `Solid Color.white`)
-- | will create a prism that violates the preview-review law. 
only :: forall a. Eq a => a -> Prism a a Unit Unit
only x = nearly x (_ == x)

-- | Create the "whole" corresponding to a specific "part":
-- |
-- | ```purescript
-- | review solidFocus Color.white == Solid Color.white
-- | ```
review :: forall s t a b. Review s t a b -> b -> t
review = under Tagged

clonePrism :: forall s t a b. APrism s t a b -> Prism s t a b
clonePrism l = withPrism l \x y p -> prism x y p

withPrism :: forall s t a b r. APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism l f = case l (Market identity Right) of
  Market g h -> f g h

matching :: forall s t a b. APrism s t a b -> s -> Either t a
matching l = withPrism l \_ f -> f

--| Ask if `preview prism` would produce a `Just`.
is :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
is l = either (const ff) (const tt) <<< matching l

--| Ask if `preview prism` would produce a `Nothing`.
isn't :: forall s t a b r. HeytingAlgebra r => APrism s t a b -> s -> r
isn't l = not <<< is l

-- Ported from Haskell: https://hackage.haskell.org/package/lens-4.16/docs/src/Control-Lens-Prism.html#below
-- | `lift` a `Prism` through a `Traversable` functor, giving a `Prism` that matches 
-- | only if all the elements of the container match the `Prism`.
-- |
-- | ``` purescript
-- | >>> [Left 1, Right "foo", Left 4, Right "woot"]^..below _Right
-- | []
-- | ```
-- | 
-- | ``` purescript
-- | >>> [Right "hail hydra!", Right "foo", Right "blah", Right "woot"]^..below _Right
-- | [["hail hydra!","foo","blah","woot"]]
-- | ```
below :: forall f s a. Traversable f => APrism' s a -> Prism' (f s) (f a)
below k =
  withPrism k $ \bt seta ->
    prism (map bt) $ \s ->
      case traverse seta s of
        Left _ -> Left s
        Right t -> Right t
