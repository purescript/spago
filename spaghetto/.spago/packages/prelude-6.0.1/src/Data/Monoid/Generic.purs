module Data.Monoid.Generic
  ( class GenericMonoid
  , genericMempty'
  , genericMempty
  ) where

import Data.Monoid (class Monoid, mempty)
import Data.Generic.Rep

class GenericMonoid a where
  genericMempty' :: a

instance genericMonoidNoArguments :: GenericMonoid NoArguments where
  genericMempty' = NoArguments

instance genericMonoidProduct :: (GenericMonoid a, GenericMonoid b) => GenericMonoid (Product a b) where
  genericMempty' = Product genericMempty' genericMempty'

instance genericMonoidConstructor :: GenericMonoid a => GenericMonoid (Constructor name a) where
  genericMempty' = Constructor genericMempty'

instance genericMonoidArgument :: Monoid a => GenericMonoid (Argument a) where
  genericMempty' = Argument mempty

-- | A `Generic` implementation of the `mempty` member from the `Monoid` type class.
genericMempty :: forall a rep. Generic a rep => GenericMonoid rep => a
genericMempty = to genericMempty'
