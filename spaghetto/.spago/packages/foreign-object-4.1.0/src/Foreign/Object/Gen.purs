module Foreign.Object.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt, resize, sized, unfoldable)
import Control.Monad.Rec.Class (class MonadRec)
import Foreign.Object (Object, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.List (List)

-- | Generates a `Object` using the specified key and value generators.
genForeignObject
  :: forall m a
  . MonadRec m
  => MonadGen m
  => m String
  -> m a
  -> m (Object a)
genForeignObject genKey genValue = sized \size -> do
  newSize <- chooseInt 0 size
  resize (const newSize) $
    (fromFoldable :: List (Tuple String a) -> Object a)
      <$> unfoldable (Tuple <$> genKey <*> genValue)
