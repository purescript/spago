module Data.Show.Generic
  ( class GenericShow
  , genericShow'
  , genericShow
  , class GenericShowArgs
  , genericShowArgs
  ) where

import Prelude (class Show, show, (<>))
import Data.Generic.Rep
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class GenericShow a where
  genericShow' :: a -> String

class GenericShowArgs a where
  genericShowArgs :: a -> Array String

instance genericShowNoConstructors :: GenericShow NoConstructors where
  genericShow' a = genericShow' a

instance genericShowArgsNoArguments :: GenericShowArgs NoArguments where
  genericShowArgs _ = []

instance genericShowSum :: (GenericShow a, GenericShow b) => GenericShow (Sum a b) where
  genericShow' (Inl a) = genericShow' a
  genericShow' (Inr b) = genericShow' b

instance genericShowArgsProduct ::
  ( GenericShowArgs a
  , GenericShowArgs b
  ) =>
  GenericShowArgs (Product a b) where
  genericShowArgs (Product a b) = genericShowArgs a <> genericShowArgs b

instance genericShowConstructor ::
  ( GenericShowArgs a
  , IsSymbol name
  ) =>
  GenericShow (Constructor name a) where
  genericShow' (Constructor a) =
    case genericShowArgs a of
      [] -> ctor
      args -> "(" <> intercalate " " ([ ctor ] <> args) <> ")"
    where
    ctor :: String
    ctor = reflectSymbol (Proxy :: Proxy name)

instance genericShowArgsArgument :: Show a => GenericShowArgs (Argument a) where
  genericShowArgs (Argument a) = [ show a ]

-- | A `Generic` implementation of the `show` member from the `Show` type class.
genericShow :: forall a rep. Generic a rep => GenericShow rep => a -> String
genericShow x = genericShow' (from x)

foreign import intercalate :: String -> Array String -> String
