module Node.EventEmitter where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn10, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7, EffectFn8, EffectFn9, mkEffectFn1, mkEffectFn10, mkEffectFn2, mkEffectFn3, mkEffectFn4, mkEffectFn5, mkEffectFn6, mkEffectFn7, mkEffectFn8, mkEffectFn9, runEffectFn10, runEffectFn2, runEffectFn3)
import Unsafe.Coerce (unsafeCoerce)

data Handlers

foreign import data CanHandle :: Handlers
foreign import data NoHandle :: Handlers

data Emittable

foreign import data CanEmit :: Emittable
foreign import data NoEmit :: Emittable

foreign import data EventEmitter :: Emittable -> Handlers -> Type

type role EventEmitter nominal nominal

foreign import new :: Effect (EventEmitter CanEmit CanHandle)

asEmitterOnly :: forall handler. EventEmitter CanEmit handler -> EventEmitter NoEmit handler
asEmitterOnly = unsafeCoerce

asHandlerOnly :: forall emit. EventEmitter emit CanHandle -> EventEmitter emit NoHandle
asHandlerOnly = unsafeCoerce

setMaxListeners :: forall a b. Int -> EventEmitter a b -> Effect Unit
setMaxListeners max emitter = runEffectFn2 setMaxListenersImpl emitter max

setUnlimitedListeners :: forall a b. EventEmitter a b -> Effect Unit
setUnlimitedListeners = setMaxListeners 0

listenersLength :: forall a b. String -> EventEmitter a b -> Effect Int
listenersLength eventName emitter = runEffectFn2 listenersLengthImpl emitter eventName

foreign import listenersLengthImpl :: forall a b. EffectFn2 (EventEmitter a b) String Int

foreign import setMaxListenersImpl :: forall a b. EffectFn2 (EventEmitter a b) Int Unit

foreign import onImpl :: forall a cb. EffectFn3 (EventEmitter a CanHandle) String cb cb
foreign import offImpl :: forall a cb. EffectFn3 (EventEmitter a CanHandle) String cb Unit

class UnsafeOnEvent callbackFn callbackEffectFn | callbackFn -> callbackEffectFn where
  unsafeOn :: forall a. String -> callbackFn -> (EventEmitter a CanHandle) -> Effect callbackEffectFn

unsafeAddEventListener
  :: forall a callbackFn callbackEffectFn
   . UnsafeOnEvent callbackFn callbackEffectFn
  => String
  -> callbackFn
  -> EventEmitter a CanHandle
  -> Effect callbackEffectFn
unsafeAddEventListener = unsafeOn

class UnsafeOffEvent callbackFn where
  unsafeOff :: forall a. String -> callbackFn -> EventEmitter a CanHandle -> Effect Unit

unsafeRemoveEventListener
  :: forall a callbackFn
   . UnsafeOffEvent callbackFn
  => String
  -> callbackFn
  -> EventEmitter a CanHandle
  -> Effect Unit
unsafeRemoveEventListener = unsafeOff

unsafeSubscribe
  :: forall a callbackFn callbackEffectFn
   . UnsafeOnEvent callbackFn callbackEffectFn
  => UnsafeOffEvent callbackEffectFn
  => String
  -> callbackFn
  -> EventEmitter a CanHandle
  -> Effect (Effect Unit)
unsafeSubscribe event cb emitter = do
  cb' <- unsafeOn event cb emitter
  pure $ unsafeOff event cb' emitter

instance UnsafeOnEvent (EffectFn10 a b c d e f g h i j Unit) (EffectFn10 a b c d e f g h i j Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn9 a b c d e f g h i Unit) (EffectFn9 a b c d e f g h i Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn8 a b c d e f g h Unit) (EffectFn8 a b c d e f g h Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn7 a b c d e f g Unit) (EffectFn7 a b c d e f g Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn6 a b c d e f Unit) (EffectFn6 a b c d e f Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn5 a b c d e Unit) (EffectFn5 a b c d e Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn4 a b c d Unit) (EffectFn4 a b c d Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn3 a b c Unit) (EffectFn3 a b c Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn2 a b Unit) (EffectFn2 a b Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn
else instance UnsafeOnEvent (EffectFn1 a Unit) (EffectFn1 a Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn

else instance UnsafeOnEvent (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Unit) (EffectFn10 a b c d e f g h i j Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn10 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Unit) (EffectFn9 a b c d e f g h i Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn9 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> d -> e -> f -> g -> h -> Effect Unit) (EffectFn8 a b c d e f g h Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn8 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> d -> e -> f -> g -> Effect Unit) (EffectFn7 a b c d e f g Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn7 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> d -> e -> f -> Effect Unit) (EffectFn6 a b c d e f Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn6 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> d -> e -> Effect Unit) (EffectFn5 a b c d e Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn5 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> d -> Effect Unit) (EffectFn4 a b c d Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn4 fn) emitter
else instance UnsafeOnEvent (a -> b -> c -> Effect Unit) (EffectFn3 a b c Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn3 fn) emitter
else instance UnsafeOnEvent (a -> b -> Effect Unit) (EffectFn2 a b Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn2 fn) emitter
else instance UnsafeOnEvent (a -> Effect Unit) (EffectFn1 a Unit) where
  unsafeOn eventName fn emitter = unsafeOn eventName (mkEffectFn1 fn) emitter
else instance UnsafeOnEvent (Effect Unit) (Effect Unit) where
  unsafeOn eventName fn emitter = runEffectFn3 onImpl emitter eventName fn

instance UnsafeOffEvent (EffectFn10 a b c d e f g h i j Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn9 a b c d e f g h i Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn8 a b c d e f g h Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn7 a b c d e f g Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn6 a b c d e f Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn5 a b c d e Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn4 a b c d Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn3 a b c Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn2 a b Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (EffectFn1 a Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn
else instance UnsafeOffEvent (Effect Unit) where
  unsafeOff eventName fn emitter = runEffectFn3 offImpl emitter eventName fn

foreign import onceEventListener :: forall a cb. EffectFn3 (EventEmitter a CanHandle) String cb Unit

class UnsafeOnceListener callbackFn where
  unsafeOnce :: forall a. String -> callbackFn -> EventEmitter a CanHandle -> Effect Unit

instance UnsafeOnceListener (EffectFn10 a b c d e f g h i j Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn9 a b c d e f g h i Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn8 a b c d e f g h Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn7 a b c d e f g Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn6 a b c d e f Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn5 a b c d e Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn4 a b c d Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn3 a b c Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn2 a b Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn
else instance UnsafeOnceListener (EffectFn1 a Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn

else instance UnsafeOnceListener (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn10 fn)
else instance UnsafeOnceListener (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn9 fn)
else instance UnsafeOnceListener (a -> b -> c -> d -> e -> f -> g -> h -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn8 fn)
else instance UnsafeOnceListener (a -> b -> c -> d -> e -> f -> g -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn7 fn)
else instance UnsafeOnceListener (a -> b -> c -> d -> e -> f -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn6 fn)
else instance UnsafeOnceListener (a -> b -> c -> d -> e -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn5 fn)
else instance UnsafeOnceListener (a -> b -> c -> d -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn4 fn)
else instance UnsafeOnceListener (a -> b -> c -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn3 fn)
else instance UnsafeOnceListener (a -> b -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn2 fn)
else instance UnsafeOnceListener (a -> Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName (mkEffectFn1 fn)
else instance UnsafeOnceListener (Effect Unit) where
  unsafeOnce eventName fn emitter =
    runEffectFn3 onceEventListener emitter eventName fn

foreign import undefined :: forall a. a

foreign import emitImpl :: forall x a b c d e f g h i j. Fn2 (EventEmitter CanEmit x) String (EffectFn10 a b c d e f g h i j Boolean)

class UnsafeEmit a where
  unsafeEmit :: forall x. (EventEmitter CanEmit x) -> String -> a

instance UnsafeEmit (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      a5
      a6
      a7
      a8
      a9
      a10
else instance UnsafeEmit (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 a5 a6 a7 a8 a9 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      a5
      a6
      a7
      a8
      a9
      undefined
else instance UnsafeEmit (a -> b -> c -> d -> e -> f -> g -> h -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 a5 a6 a7 a8 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      a5
      a6
      a7
      a8
      undefined
      undefined
else instance UnsafeEmit (a -> b -> c -> d -> e -> f -> g -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 a5 a6 a7 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      a5
      a6
      a7
      undefined
      undefined
      undefined
else instance UnsafeEmit (a -> b -> c -> d -> e -> f -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 a5 a6 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      a5
      a6
      undefined
      undefined
      undefined
      undefined
else instance UnsafeEmit (a -> b -> c -> d -> e -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 a5 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      a5
      undefined
      undefined
      undefined
      undefined
      undefined
else instance UnsafeEmit (a -> b -> c -> d -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 a4 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      a4
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
else instance UnsafeEmit (a -> b -> c -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 a3 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      a3
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
else instance UnsafeEmit (a -> b -> Effect Boolean) where
  unsafeEmit emitter eventName a1 a2 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      a2
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
else instance UnsafeEmit (a -> Effect Boolean) where
  unsafeEmit emitter eventName a1 = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      a1
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
else instance UnsafeEmit (Effect Boolean) where
  unsafeEmit emitter eventName = do
    runEffectFn10 (runFn2 emitImpl emitter eventName)
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
      undefined
