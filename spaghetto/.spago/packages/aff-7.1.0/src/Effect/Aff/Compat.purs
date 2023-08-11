-- | This module provides compatability functions for constructing `Aff`s which
-- | are defined via the FFI.
module Effect.Aff.Compat
  ( EffectFnAff(..)
  , EffectFnCanceler(..)
  , EffectFnCb
  , fromEffectFnAff
  , module Effect.Uncurried
  ) where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), makeAff, nonCanceler)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

type EffectFnCb a = EffectFn1 a Unit

newtype EffectFnAff a = EffectFnAff (EffectFn2 (EffectFnCb Error) (EffectFnCb a) EffectFnCanceler)

newtype EffectFnCanceler = EffectFnCanceler (EffectFn3 Error (EffectFnCb Error) (EffectFnCb Unit) Unit)

-- | Lift a FFI definition into an `Aff`. `EffectFnAff` makes use of `EffectFn` so
-- | `Effect` thunks are unnecessary. A definition might follow this example:
-- |
-- | ```javascript
-- | exports._myAff = function (onError, onSuccess) {
-- |   var cancel = doSomethingAsync(function (err, res) {
-- |     if (err) {
-- |       onError(err);
-- |     } else {
-- |       onSuccess(res);
-- |     }
-- |   });
-- |   return function (cancelError, onCancelerError, onCancelerSuccess) {
-- |     cancel();
-- |     onCancelerSuccess();
-- |   };
-- | };
-- | ```
-- |
-- | ```purescript
-- | foreign import _myAff :: EffectFnAff String
-- |
-- | myAff :: Aff String
-- | myAff = fromEffectFnAff _myAff
-- | ````
fromEffectFnAff :: EffectFnAff ~> Aff
fromEffectFnAff (EffectFnAff eff) = makeAff \k -> do
  EffectFnCanceler canceler <- runEffectFn2 eff (mkEffectFn1 (k <<< Left)) (mkEffectFn1 (k <<< Right))
  pure $ Canceler \e -> makeAff \k2 -> do
    runEffectFn3 canceler e (mkEffectFn1 (k2 <<< Left)) (mkEffectFn1 (k2 <<< Right))
    pure nonCanceler
