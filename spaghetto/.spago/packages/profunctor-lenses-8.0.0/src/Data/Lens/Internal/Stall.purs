-- | This module defines the `Stall` profunctor
module Data.Lens.Internal.Stall where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))

-- | The `Stall` profunctor characterizes an `AffineTraversal`.
data Stall a b s t = Stall (s -> b -> t) (s -> Either t a)

instance functorStall :: Functor (Stall a b s) where
  map f (Stall u p) =
    Stall (map f <<< u) (lmap f <<< p)

instance profunctorStall :: Profunctor (Stall a b) where
  dimap f g (Stall u p) =
    Stall (map g <<< u <<< f) (lmap g <<< p <<< f)

instance strongStall :: Strong (Stall a b) where
  first (Stall u p) =
    Stall (\(Tuple s x) b -> Tuple (u s b) x)
      (\(Tuple s x) -> lmap (\t -> Tuple t x) (p s))

  second (Stall u p) =
    Stall (\(Tuple x s) b -> Tuple x (u s b))
      (\(Tuple x s) -> lmap (Tuple x) (p s))

instance choiceStall :: Choice (Stall a b) where
  left (Stall u p) =
    Stall
      ( case _ of
          Left s -> \b -> Left (u s b)
          Right x -> \_ -> Right x
      )
      ( case _ of
          Left s -> lmap Left (p s)
          Right x -> Left (Right x)
      )

  right (Stall u p) =
    Stall
      ( case _ of
          Left x -> \_ -> Left x
          Right s -> \b -> Right (u s b)
      )
      ( case _ of
          Left x -> Left (Left x)
          Right s -> lmap Right (p s)
      )
