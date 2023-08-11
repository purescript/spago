module Effect.AVar
  ( AVar
  , AVarCallback
  , AVarStatus(..)
  , new
  , empty
  , take
  , tryTake
  , put
  , tryPut
  , read
  , tryRead
  , kill
  , status
  , isEmpty
  , isFilled
  , isKilled
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error)

type AVarCallback a = (Either Error a -> Effect Unit)

foreign import data AVar :: Type -> Type

type role AVar representational

data AVarStatus a
  = Killed Error
  | Filled a
  | Empty

-- | Creates a new empty AVar.
foreign import empty :: forall a. Effect (AVar a)

-- | Creates a fresh AVar with an initial value.
new :: forall a. a -> Effect (AVar a)
new = _newVar

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill :: forall a. Error -> AVar a -> Effect Unit
kill err avar = Fn.runFn3 _killVar ffiUtil err avar

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
put :: forall a. a -> AVar a -> AVarCallback Unit -> Effect (Effect Unit)
put value avar cb = Fn.runFn4 _putVar ffiUtil value avar cb

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut :: forall a. a -> AVar a -> Effect Boolean
tryPut value avar = Fn.runFn3 _tryPutVar ffiUtil value avar

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
take :: forall a. AVar a -> AVarCallback a -> Effect (Effect Unit)
take avar cb = Fn.runFn3 _takeVar ffiUtil avar cb

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake :: forall a. AVar a -> Effect (Maybe a)
tryTake avar = Fn.runFn2 _tryTakeVar ffiUtil avar

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read :: forall a. AVar a -> AVarCallback a -> Effect (Effect Unit)
read avar cb = Fn.runFn3 _readVar ffiUtil avar cb

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead :: forall a. AVar a -> Effect (Maybe a)
tryRead avar = Fn.runFn2 _tryReadVar ffiUtil avar

-- | Synchronously checks the status of an AVar.
status :: forall a. AVar a -> Effect (AVarStatus a)
status avar = Fn.runFn2 _status ffiUtil avar

isEmpty :: forall a. AVarStatus a -> Boolean
isEmpty = case _ of
  Empty -> true
  _ -> false

isFilled :: forall a. AVarStatus a -> Boolean
isFilled = case _ of
  Filled _ -> true
  _ -> false

isKilled :: forall a. AVarStatus a -> Boolean
isKilled = case _ of
  Killed _ -> true
  _ -> false

foreign import _newVar :: forall a. a -> Effect (AVar a)
foreign import _killVar :: forall a. Fn.Fn3 FFIUtil Error (AVar a) (Effect Unit)
foreign import _putVar :: forall a. Fn.Fn4 FFIUtil a (AVar a) (AVarCallback Unit) (Effect (Effect Unit))
foreign import _tryPutVar :: forall a. Fn.Fn3 FFIUtil a (AVar a) (Effect Boolean)
foreign import _takeVar :: forall a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback a) (Effect (Effect Unit))
foreign import _tryTakeVar :: forall a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe a))
foreign import _readVar :: forall a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback a) (Effect (Effect Unit))
foreign import _tryReadVar :: forall a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe a))
foreign import _status :: forall a. Fn.Fn2 FFIUtil (AVar a) (Effect (AVarStatus a))

type FFIUtil =
  { left :: forall a b. a -> Either a b
  , right :: forall a b. b -> Either a b
  , nothing :: forall a. Maybe a
  , just :: forall a. a -> Maybe a
  , killed :: forall a. Error -> AVarStatus a
  , filled :: forall a. a -> AVarStatus a
  , empty :: forall a. AVarStatus a
  }

ffiUtil :: FFIUtil
ffiUtil =
  { left: Left
  , right: Right
  , nothing: Nothing
  , just: Just
  , killed: Killed
  , filled: Filled
  , empty: Empty
  }
