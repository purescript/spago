module Data.Generic.Rep
  ( class Generic
  , to
  , from
  , repOf
  , NoConstructors
  , NoArguments(..)
  , Sum(..)
  , Product(..)
  , Constructor(..)
  , Argument(..)
  ) where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Void (Void)
import Type.Proxy (Proxy(..))

-- | A representation for types with no constructors.
newtype NoConstructors = NoConstructors Void

-- | A representation for constructors with no arguments.
data NoArguments = NoArguments

instance showNoArguments :: Show NoArguments where
  show _ = "NoArguments"

-- | A representation for types with multiple constructors.
data Sum a b = Inl a | Inr b

instance showSum :: (Show a, Show b) => Show (Sum a b) where
  show (Inl a) = "(Inl " <> show a <> ")"
  show (Inr b) = "(Inr " <> show b <> ")"

-- | A representation for constructors with multiple fields.
data Product a b = Product a b

instance showProduct :: (Show a, Show b) => Show (Product a b) where
  show (Product a b) = "(Product " <> show a <> " " <> show b <> ")"

-- | A representation for constructors which includes the data constructor name
-- | as a type-level string.
newtype Constructor (name :: Symbol) a = Constructor a

instance showConstructor :: (IsSymbol name, Show a) => Show (Constructor name a) where
  show (Constructor a) = "(Constructor @" <> show (reflectSymbol (Proxy :: Proxy name)) <> " " <> show a <> ")"

-- | A representation for an argument in a data constructor.
newtype Argument a = Argument a

instance showArgument :: Show a => Show (Argument a) where
  show (Argument a) = "(Argument " <> show a <> ")"

-- | The `Generic` class asserts the existence of a type function from types
-- | to their representations using the type constructors defined in this module.
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep

repOf :: forall a rep. Generic a rep => Proxy a -> Proxy rep
repOf _ = Proxy
