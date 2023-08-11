module Node.EventEmitter.TypedEmitter
  ( TypedEmitter
  , new
  , asEmitterOnly
  , asHandlerOnly
  , setMaxListeners
  , setUnlimitedListeners
  , listenersLength
  , on
  , addEventListener
  , off
  , removeEventListener
  , subscribe
  , once
  , withEmit
  , emit
  , class EmitterFunction
  , class HandlerFunction
  , module Exports
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn10, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7, EffectFn8, EffectFn9)
import Node.EventEmitter (class UnsafeEmit, class UnsafeOffEvent, class UnsafeOnEvent, class UnsafeOnceListener, CanEmit, CanHandle, Emittable, EventEmitter, Handlers, NoEmit, NoHandle, unsafeEmit, unsafeOff, unsafeOn, unsafeOnce, unsafeSubscribe)
import Node.EventEmitter (class UnsafeEmit, class UnsafeOffEvent, class UnsafeOnEvent, class UnsafeOnceListener, CanEmit, CanHandle, Emittable, Handlers, NoEmit, NoHandle) as Exports
import Node.EventEmitter as EventEmitter
import Prim.Row as Row
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)

newtype TypedEmitter :: Emittable -> Handlers -> Row Type -> Type
newtype TypedEmitter e h r = TypedEmitter (EventEmitter e h)

type role TypedEmitter nominal nominal representational

new
  :: forall r all
   . Proxy r
  -> Effect (TypedEmitter CanEmit CanHandle all)
new _ = (coerce :: Effect (EventEmitter CanEmit CanHandle) -> Effect (TypedEmitter CanEmit CanHandle all)) EventEmitter.new

asEmitterOnly :: forall handler all. TypedEmitter CanEmit handler all -> TypedEmitter NoEmit handler all
asEmitterOnly (TypedEmitter emitter) = TypedEmitter $ EventEmitter.asEmitterOnly emitter

asHandlerOnly :: forall emit all. TypedEmitter emit CanHandle all -> TypedEmitter emit NoHandle all
asHandlerOnly (TypedEmitter emitter) = TypedEmitter $ EventEmitter.asHandlerOnly emitter

setMaxListeners :: forall emit handler row. Int -> TypedEmitter emit handler row -> Effect Unit
setMaxListeners max (TypedEmitter emitter) = EventEmitter.setMaxListeners max emitter

setUnlimitedListeners :: forall emit handler row. TypedEmitter emit handler row -> Effect Unit
setUnlimitedListeners (TypedEmitter emitter) = EventEmitter.setUnlimitedListeners emitter

listenersLength
  :: forall e h sym a tail row
   . Row.Cons sym a tail row
  => IsSymbol sym
  => Proxy sym
  -> TypedEmitter e h row
  -> Effect Int
listenersLength _sym (TypedEmitter emitter) = EventEmitter.listenersLength (reflectSymbol _sym) emitter

on
  :: forall emit sym fn tail row effectFn
   . Row.Cons sym fn tail row
  => UnsafeOnEvent fn effectFn
  => IsSymbol sym
  => Proxy sym
  -> fn
  -> TypedEmitter emit CanHandle row
  -> Effect effectFn
on _sym fn (TypedEmitter emitter) = unsafeOn (reflectSymbol _sym) fn emitter

addEventListener
  :: forall emit sym fn tail row effectFn
   . Row.Cons sym fn tail row
  => UnsafeOnEvent fn effectFn
  => IsSymbol sym
  => Proxy sym
  -> fn
  -> TypedEmitter emit CanHandle row
  -> Effect effectFn
addEventListener = on

off
  :: forall emit sym fn tail row effectFn
   . Row.Cons sym fn tail row
  => HandlerFunction fn effectFn
  => UnsafeOffEvent effectFn
  => IsSymbol sym
  => Proxy sym
  -> effectFn
  -> TypedEmitter emit CanHandle row
  -> Effect Unit
off _sym fn (TypedEmitter emitter) = unsafeOff (reflectSymbol _sym) fn emitter

removeEventListener
  :: forall emit sym fn tail row effectFn
   . Row.Cons sym fn tail row
  => HandlerFunction fn effectFn
  => UnsafeOffEvent effectFn
  => IsSymbol sym
  => Proxy sym
  -> effectFn
  -> TypedEmitter emit CanHandle row
  -> Effect Unit
removeEventListener = off

subscribe
  :: forall emit sym fn tail row effectFn
   . Row.Cons sym fn tail row
  => UnsafeOnEvent fn effectFn
  => UnsafeOffEvent effectFn
  => IsSymbol sym
  => Proxy sym
  -> fn
  -> TypedEmitter emit CanHandle row
  -> Effect (Effect Unit)
subscribe _sym fn (TypedEmitter emitter) = unsafeSubscribe (reflectSymbol _sym) fn emitter

once
  :: forall emit sym fn tail row
   . Row.Cons sym fn tail row
  => UnsafeOnceListener fn
  => IsSymbol sym
  => Proxy sym
  -> fn
  -> TypedEmitter emit CanHandle row
  -> Effect Unit
once _sym fn (TypedEmitter emitter) = unsafeOnce (reflectSymbol _sym) fn emitter

-- | Helps with type inference
-- | due to the overloaded nature of `emit`.
withEmit :: Effect Boolean -> Effect Boolean
withEmit = identity

-- | Since the return type is polymorphic to allow one to
-- | easily add as many arguments as are needed
-- | using the same function,
-- | type inference suffers. To workaround that
-- | issue, precede all calls with `withEmit`.
-- | For example
-- | ```
-- | b <- withEmit $ emit (Proxy :: _ "eventName") emitter arg1 arg2 arg3
-- | void $ withEmit $ emit (Proxy :: _ "eventName") emitter arg1 arg2 arg3
-- | ```
-- | Otherwise, you'll have to write this to deal with compiler errors:
-- | ```
-- | (_ :: Boolean) <- emit (Proxy :: _ "eventName") emitter arg1 arg2 arg3
-- | map (\(_ :: Boolean) -> unit) $ emit (Proxy :: _ "eventName") emitter arg1 arg2 arg3
-- | ```
emit
  :: forall handler sym fn tail row argsThenEffectBoolean
   . Row.Cons sym fn tail row
  => EmitterFunction fn argsThenEffectBoolean
  => UnsafeEmit argsThenEffectBoolean
  => IsSymbol sym
  => Proxy sym
  -> TypedEmitter CanEmit handler row
  -> argsThenEffectBoolean
emit _sym (TypedEmitter emitter) = unsafeEmit emitter (reflectSymbol _sym)

-- | Determines the number and type of args to pass into the `emit` function
-- | based on the number and type of args received by the callback type
-- | associated with that label in the row.
class EmitterFunction :: Type -> Type -> Constraint
class EmitterFunction callbackFn emitterArgs | callbackFn -> emitterArgs

instance EmitterFunction (EffectFn10 a b c d e f g h i j Unit) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Boolean)
else instance EmitterFunction (EffectFn9 a b c d e f g h i Unit) (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Boolean)
else instance EmitterFunction (EffectFn8 a b c d e f g h Unit) (a -> b -> c -> d -> e -> f -> g -> h -> Effect Boolean)
else instance EmitterFunction (EffectFn7 a b c d e f g Unit) (a -> b -> c -> d -> e -> f -> g -> Effect Boolean)
else instance EmitterFunction (EffectFn6 a b c d e f Unit) (a -> b -> c -> d -> e -> f -> Effect Boolean)
else instance EmitterFunction (EffectFn5 a b c d e Unit) (a -> b -> c -> d -> e -> Effect Boolean)
else instance EmitterFunction (EffectFn4 a b c d Unit) (a -> b -> c -> d -> Effect Boolean)
else instance EmitterFunction (EffectFn3 a b c Unit) (a -> b -> c -> Effect Boolean)
else instance EmitterFunction (EffectFn2 a b Unit) (a -> b -> Effect Boolean)
else instance EmitterFunction (EffectFn1 a Unit) (a -> Effect Boolean)

else instance EmitterFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Unit) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Unit) (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> d -> e -> f -> g -> h -> Effect Unit) (a -> b -> c -> d -> e -> f -> g -> h -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> d -> e -> f -> g -> Effect Unit) (a -> b -> c -> d -> e -> f -> g -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> d -> e -> f -> Effect Unit) (a -> b -> c -> d -> e -> f -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> d -> e -> Effect Unit) (a -> b -> c -> d -> e -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> d -> Effect Unit) (a -> b -> c -> d -> Effect Boolean)
else instance EmitterFunction (a -> b -> c -> Effect Unit) (a -> b -> c -> Effect Boolean)
else instance EmitterFunction (a -> b -> Effect Unit) (a -> b -> Effect Boolean)
else instance EmitterFunction (a -> Effect Unit) (a -> Effect Boolean)
else instance EmitterFunction (Effect Unit) (Effect Boolean)

class HandlerFunction :: Type -> Type -> Constraint
class HandlerFunction callbackFn handlerFn | callbackFn -> handlerFn

instance HandlerFunction (EffectFn10 a b c d e f g h i j Unit) (EffectFn10 a b c d e f g h i j Unit)
else instance HandlerFunction (EffectFn9 a b c d e f g h i Unit) (EffectFn9 a b c d e f g h i Unit)
else instance HandlerFunction (EffectFn8 a b c d e f g h Unit) (EffectFn8 a b c d e f g h Unit)
else instance HandlerFunction (EffectFn7 a b c d e f g Unit) (EffectFn7 a b c d e f g Unit)
else instance HandlerFunction (EffectFn6 a b c d e f Unit) (EffectFn6 a b c d e f Unit)
else instance HandlerFunction (EffectFn5 a b c d e Unit) (EffectFn5 a b c d e Unit)
else instance HandlerFunction (EffectFn4 a b c d Unit) (EffectFn4 a b c d Unit)
else instance HandlerFunction (EffectFn3 a b c Unit) (EffectFn3 a b c Unit)
else instance HandlerFunction (EffectFn2 a b Unit) (EffectFn2 a b Unit)
else instance HandlerFunction (EffectFn1 a Unit) (EffectFn1 a Unit)
else instance HandlerFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Boolean) (EffectFn10 a b c d e f g h i j Unit)
else instance HandlerFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Boolean) (EffectFn9 a b c d e f g h i Unit)
else instance HandlerFunction (a -> b -> c -> d -> e -> f -> g -> h -> Effect Boolean) (EffectFn8 a b c d e f g h Unit)
else instance HandlerFunction (a -> b -> c -> d -> e -> f -> g -> Effect Boolean) (EffectFn7 a b c d e f g Unit)
else instance HandlerFunction (a -> b -> c -> d -> e -> f -> Effect Boolean) (EffectFn6 a b c d e f Unit)
else instance HandlerFunction (a -> b -> c -> d -> e -> Effect Boolean) (EffectFn5 a b c d e Unit)
else instance HandlerFunction (a -> b -> c -> d -> Effect Boolean) (EffectFn4 a b c d Unit)
else instance HandlerFunction (a -> b -> c -> Effect Boolean) (EffectFn3 a b c Unit)
else instance HandlerFunction (a -> b -> Effect Boolean) (EffectFn2 a b Unit)
else instance HandlerFunction (a -> Effect Boolean) (EffectFn1 a Unit)
else instance HandlerFunction (Effect Unit) (Effect Unit)
