module Web.HTML.ValidityState
  ( ValidityState
  , valueMissing
  , typeMismatch
  , patternMismatch
  , tooLong
  , tooShort
  , rangeUnderflow
  , rangeOverflow
  , stepMismatch
  , badInput
  , customError
  , valid
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import data ValidityState :: Type

valueMissing :: ValidityState -> Effect Boolean
valueMissing = runEffectFn2 readProp "valueMissing"

typeMismatch :: ValidityState -> Effect Boolean
typeMismatch = runEffectFn2 readProp "typeMismatch"

patternMismatch :: ValidityState -> Effect Boolean
patternMismatch = runEffectFn2 readProp "patternMismatch"

tooLong :: ValidityState -> Effect Boolean
tooLong = runEffectFn2 readProp "tooLong"

tooShort :: ValidityState -> Effect Boolean
tooShort = runEffectFn2 readProp "tooShort"

rangeUnderflow :: ValidityState -> Effect Boolean
rangeUnderflow = runEffectFn2 readProp "rangeUnderflow"

rangeOverflow :: ValidityState -> Effect Boolean
rangeOverflow = runEffectFn2 readProp "rangeOverflow"

stepMismatch :: ValidityState -> Effect Boolean
stepMismatch = runEffectFn2 readProp "stepMismatch"

badInput :: ValidityState -> Effect Boolean
badInput = runEffectFn2 readProp "badInput"

customError :: ValidityState -> Effect Boolean
customError = runEffectFn2 readProp "customError"

valid :: ValidityState -> Effect Boolean
valid = runEffectFn2 readProp "valid"

foreign import readProp :: EffectFn2 String ValidityState Boolean
