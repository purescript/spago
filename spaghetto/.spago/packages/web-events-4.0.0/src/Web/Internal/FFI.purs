module Web.Internal.FFI (unsafeReadProtoTagged) where

import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))

unsafeReadProtoTagged :: forall a b. String -> a -> Maybe b
unsafeReadProtoTagged name value =
  runFn4 _unsafeReadProtoTagged Nothing Just name value

foreign import _unsafeReadProtoTagged
  :: forall a b
   . Fn4
      (forall x. Maybe x)
      (forall x. x -> Maybe x)
      String
      a
      (Maybe b)
