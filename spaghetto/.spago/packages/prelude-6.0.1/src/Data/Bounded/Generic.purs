module Data.Bounded.Generic
  ( class GenericBottom
  , genericBottom'
  , genericBottom
  , class GenericTop
  , genericTop'
  , genericTop
  ) where

import Data.Generic.Rep

import Data.Bounded (class Bounded, bottom, top)

class GenericBottom a where
  genericBottom' :: a

instance genericBottomNoArguments :: GenericBottom NoArguments where
  genericBottom' = NoArguments

instance genericBottomArgument :: Bounded a => GenericBottom (Argument a) where
  genericBottom' = Argument bottom

instance genericBottomSum :: GenericBottom a => GenericBottom (Sum a b) where
  genericBottom' = Inl genericBottom'

instance genericBottomProduct :: (GenericBottom a, GenericBottom b) => GenericBottom (Product a b) where
  genericBottom' = Product genericBottom' genericBottom'

instance genericBottomConstructor :: GenericBottom a => GenericBottom (Constructor name a) where
  genericBottom' = Constructor genericBottom'

class GenericTop a where
  genericTop' :: a

instance genericTopNoArguments :: GenericTop NoArguments where
  genericTop' = NoArguments

instance genericTopArgument :: Bounded a => GenericTop (Argument a) where
  genericTop' = Argument top

instance genericTopSum :: GenericTop b => GenericTop (Sum a b) where
  genericTop' = Inr genericTop'

instance genericTopProduct :: (GenericTop a, GenericTop b) => GenericTop (Product a b) where
  genericTop' = Product genericTop' genericTop'

instance genericTopConstructor :: GenericTop a => GenericTop (Constructor name a) where
  genericTop' = Constructor genericTop'

-- | A `Generic` implementation of the `bottom` member from the `Bounded` type class.
genericBottom :: forall a rep. Generic a rep => GenericBottom rep => a
genericBottom = to genericBottom'

-- | A `Generic` implementation of the `top` member from the `Bounded` type class.
genericTop :: forall a rep. Generic a rep => GenericTop rep => a
genericTop = to genericTop'
