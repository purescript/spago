module Control.Promise (fromAff, toAff, toAff', toAffE, Promise()) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (Foreign, readString, unsafeReadTagged)

-- | Type of JavaScript Promises (with particular return type)
-- | Effects are not traced in the Promise type, as they form part of the Effect that
-- | results in the promise.
foreign import data Promise :: Type -> Type

type role Promise representational

foreign import promise :: forall a b.
  ((a -> Effect Unit) -> (b -> Effect Unit) -> Effect Unit) -> Effect (Promise a)
foreign import thenImpl :: forall a b.
  Promise a -> (EffectFn1 Foreign b) -> (EffectFn1 a b) -> Effect Unit

-- | Convert an Aff into a Promise.
fromAff :: forall a. Aff a -> Effect (Promise a)
fromAff aff = promise (\succ err -> runAff_ (either err succ) aff)

coerce :: Foreign -> Error
coerce fn =
  either (\_ -> error "Promise failed, couldn't extract JS Error or String")
         identity
         (runExcept ((unsafeReadTagged "Error" fn) <|> (error <$> readString fn)))

-- | Convert a Promise into an Aff.
-- | When the promise rejects, we attempt to
-- | coerce the error value into an actual JavaScript Error object. We can do this
-- | with Error objects or Strings. Anything else gets a "dummy" Error object.
toAff :: forall a. Promise a -> Aff a
toAff = toAff' coerce

-- | Convert a Promise into an Aff with custom Error coercion.
-- | When the promise rejects, we attempt to coerce the error value into an
-- | actual JavaScript Error object using the provided function.
toAff' :: forall a. (Foreign -> Error) -> Promise a -> Aff a
toAff' customCoerce p = makeAff
  (\cb -> mempty <$ thenImpl
    p
    (mkEffectFn1 $ cb <<< Left <<< customCoerce)
    (mkEffectFn1 $ cb <<< Right))

-- | Utility to convert an Effect returning a Promise into an Aff (i.e. the inverse of fromAff)
toAffE :: forall a. Effect (Promise a) -> Aff a
toAffE f = liftEffect f >>= toAff
