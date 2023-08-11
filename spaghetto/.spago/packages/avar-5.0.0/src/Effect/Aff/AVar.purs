module Effect.Aff.AVar
  ( module Effect.AVar
  , new
  , empty
  , status
  , take
  , tryTake
  , put
  , tryPut
  , read
  , tryRead
  , kill
  ) where

import Prelude
import Data.Maybe (Maybe)
import Effect.Aff (Aff, makeAff, effectCanceler)
import Effect.AVar (AVar, AVarStatus(..), isEmpty, isFilled, isKilled)
import Effect.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (Error)

-- | Creates a fresh AVar with an initial value.
new :: forall a. a -> Aff (AVar a)
new = liftEffect <<< AVar.new

-- | Creates a fresh AVar.
empty :: forall a. Aff (AVar a)
empty = liftEffect AVar.empty

-- | Synchronously checks the status of an AVar.
status :: forall a. AVar a -> Aff (AVar.AVarStatus a)
status = liftEffect <<< AVar.status

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills.
take :: forall a. AVar a -> Aff a
take avar = makeAff \k -> do
  c <- AVar.take avar k
  pure (effectCanceler c)

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake :: forall a. AVar a -> Aff (Maybe a)
tryTake = liftEffect <<< AVar.tryTake

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available.
put :: forall a. a -> AVar a -> Aff Unit
put value avar = makeAff \k -> do
  c <- AVar.put value avar k
  pure (effectCanceler c)

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut :: forall a. a -> AVar a -> Aff Boolean
tryPut value = liftEffect <<< AVar.tryPut value

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read :: forall a. AVar a -> Aff a
read avar = makeAff \k -> do
  c <- AVar.read avar k
  pure (effectCanceler c)

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead :: forall a. AVar a -> Aff (Maybe a)
tryRead = liftEffect <<< AVar.tryRead

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill :: forall a. Error -> AVar a -> Aff Unit
kill error = liftEffect <<< AVar.kill error
