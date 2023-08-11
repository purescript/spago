-- | This module provides a low-level wrapper for the [Node Stream API](https://nodejs.org/api/stream.html).

module Node.Stream
  ( Stream()
  , Read()
  , Readable()
  , Write()
  , Writable()
  , Duplex()
  , onData
  , onDataString
  , onDataEither
  , setEncoding
  , onReadable
  , onEnd
  , onFinish
  , onClose
  , onError
  , resume
  , pause
  , isPaused
  , pipe
  , unpipe
  , unpipeAll
  , read
  , readString
  , readEither
  , write
  , writeString
  , cork
  , uncork
  , setDefaultEncoding
  , end
  , destroy
  , destroyWithError
  ) where

import Prelude

import Effect (Effect)
import Effect.Exception (throw, Error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Node.Buffer (Buffer)
import Data.Nullable as N
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)

-- | A stream.
-- |
-- | The type arguments track, in order:
-- |
-- | - Whether reading and/or writing from/to the stream are allowed.
-- | - Effects associated with reading/writing from/to this stream.
foreign import data Stream :: Row Type -> Type

-- | A phantom type associated with _readable streams_.
data Read

-- | A readable stream.
type Readable r = Stream (read :: Read | r)

-- | A phantom type associated with _writable streams_.
data Write

-- | A writable stream.
type Writable r = Stream (write :: Write | r)

-- | A duplex (readable _and_ writable stream)
type Duplex = Stream (read :: Read, write :: Write)

foreign import undefined :: forall a. a

foreign import data Chunk :: Type

foreign import readChunkImpl
  :: (forall l r. l -> Either l r)
  -> (forall l r. r -> Either l r)
  -> Chunk
  -> Either String Buffer

readChunk :: Chunk -> Either String Buffer
readChunk = readChunkImpl Left Right

-- | Listen for `data` events, returning data in a Buffer. Note that this will fail
-- | if `setEncoding` has been called on the stream.
onData
  :: forall w
   . Readable w
  -> (Buffer -> Effect Unit)
  -> Effect Unit
onData r cb =
  onDataEither r (cb <=< fromEither)
  where
  fromEither x =
    case x of
      Left _  ->
        throw "Stream encoding should not be set"
      Right buf ->
        pure buf

read
  :: forall w
   . Readable w
   -> Maybe Int
   -> Effect (Maybe Buffer)
read r size = do
  v <- readEither r size
  case v of
    Nothing        -> pure Nothing
    Just (Left _)  -> throw "Stream encoding should not be set"
    Just (Right b) -> pure (Just b)

readString
  :: forall w
   . Readable w
  -> Maybe Int
  -> Encoding
  -> Effect (Maybe String)
readString r size enc = do
  v <- readEither r size
  case v of
       Nothing          -> pure Nothing
       Just (Left _)    -> throw "Stream encoding should not be set"
       Just (Right buf) -> Just <$> Buffer.toString enc buf

readEither
  :: forall w
   . Readable w
  -> Maybe Int
  -> Effect (Maybe (Either String Buffer))
readEither r size = readImpl readChunk Nothing Just r (fromMaybe undefined size)

foreign import readImpl
  :: forall r
   . (Chunk -> Either String Buffer)
  -> (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Readable r
  -> Int
  -> Effect (Maybe (Either String Buffer))

-- | Listen for `data` events, returning data in a String, which will be
-- | decoded using the given encoding. Note that this will fail if `setEncoding`
-- | has been called on the stream.
onDataString
  :: forall w
   . Readable w
  -> Encoding
  -> (String -> Effect Unit)
  -> Effect Unit
onDataString r enc cb = onData r (cb <=< Buffer.toString enc)

-- | Listen for `data` events, returning data in an `Either String Buffer`. This
-- | function is provided for the (hopefully rare) case that `setEncoding` has
-- | been called on the stream.
onDataEither
  :: forall r
   . Readable r
  -> (Either String Buffer -> Effect Unit)
  -> Effect Unit
onDataEither r cb = onDataEitherImpl readChunk r cb

foreign import onDataEitherImpl
  :: forall r
   . (Chunk -> Either String Buffer)
  -> Readable r
  -> (Either String Buffer -> Effect Unit)
  -> Effect Unit

foreign import setEncodingImpl
  :: forall w
   . Readable w
  -> String
  -> Effect Unit

-- | Set the encoding used to read chunks as strings from the stream. This
-- | function may be useful when you are passing a readable stream to some other
-- | JavaScript library, which already expects an encoding to be set.
-- |
-- | Where possible, you should try to use `onDataString` instead of this
-- | function.
setEncoding
  :: forall w
   . Readable w
  -> Encoding
  -> Effect Unit
setEncoding r enc = setEncodingImpl r (show enc)

-- | Listen for `readable` events.
foreign import onReadable
  :: forall w
   . Readable w
  -> Effect Unit
  -> Effect Unit

-- | Listen for `end` events.
foreign import onEnd
  :: forall w
   . Readable w
  -> Effect Unit
  -> Effect Unit

-- | Listen for `finish` events.
foreign import onFinish
  :: forall w
   . Writable w
  -> Effect Unit
  -> Effect Unit

-- | Listen for `close` events.
foreign import onClose
  :: forall w
   . Stream w
  -> Effect Unit
  -> Effect Unit

-- | Listen for `error` events.
foreign import onError
  :: forall w
   . Stream w
  -> (Error -> Effect Unit)
  -> Effect Unit

-- | Resume reading from the stream.
foreign import resume :: forall w. Readable w -> Effect Unit

-- | Pause reading from the stream.
foreign import pause :: forall w. Readable w -> Effect Unit

-- | Check whether or not a stream is paused for reading.
foreign import isPaused :: forall w. Readable w -> Effect Boolean

-- | Read chunks from a readable stream and write them to a writable stream.
foreign import pipe
  :: forall r w
   . Readable w
  -> Writable r
  -> Effect (Writable r)

-- | Detach a Writable stream previously attached using `pipe`.
foreign import unpipe
  :: forall r w
   . Readable w
  -> Writable r
  -> Effect Unit

-- | Detach all Writable streams previously attached using `pipe`.
foreign import unpipeAll
  :: forall w
   . Readable w
  -> Effect Unit

foreign import writeImpl
  :: forall r
   . Writable r
  -> Buffer
  -> EffectFn1 (N.Nullable Error) Unit
  -> Effect Boolean

-- | Write a Buffer to a writable stream.
write
  :: forall r
   . Writable r
  -> Buffer
  -> (Maybe Error -> Effect Unit)
  -> Effect Boolean
write w b cb = writeImpl w b $ mkEffectFn1 (cb <<< N.toMaybe)

foreign import writeStringImpl
  :: forall r
   . Writable r
  -> String
  -> String
  -> EffectFn1 (N.Nullable Error) Unit
  -> Effect Boolean

-- | Write a string in the specified encoding to a writable stream.
writeString
  :: forall r
   . Writable r
  -> Encoding
  -> String
  -> (Maybe Error -> Effect Unit)
  -> Effect Boolean
writeString w enc s cb = writeStringImpl w (show enc) s $ mkEffectFn1 (cb <<< N.toMaybe)

-- | Force buffering of writes.
foreign import cork :: forall r. Writable r -> Effect Unit

-- | Flush buffered data.
foreign import uncork :: forall r. Writable r -> Effect Unit

foreign import setDefaultEncodingImpl
  :: forall r
   . Writable r
  -> String
  -> Effect Unit

-- | Set the default encoding used to write strings to the stream. This function
-- | is useful when you are passing a writable stream to some other JavaScript
-- | library, which already expects a default encoding to be set. It has no
-- | effect on the behaviour of the `writeString` function (because that
-- | function ensures that the encoding is always supplied explicitly).
setDefaultEncoding
  :: forall r
   . Writable r
  -> Encoding
  -> Effect Unit
setDefaultEncoding r enc = setDefaultEncodingImpl r (show enc)

foreign import endImpl
  :: forall r
   . Writable r
  -> EffectFn1 (N.Nullable Error) Unit
  -> Effect Unit

-- | End writing data to the stream.
end
  :: forall r
   . Writable r
  -> (Maybe Error -> Effect Unit)
  -> Effect Unit
end w cb = endImpl w $ mkEffectFn1 (cb <<< N.toMaybe)

-- | Destroy the stream. It will release any internal resources.
--
-- Added in node 8.0.
foreign import destroy
  :: forall r
   . Stream r
  -> Effect Unit

-- | Destroy the stream and emit 'error'.
--
-- Added in node 8.0.
foreign import destroyWithError
  :: forall r
   . Stream r
  -> Error
  -> Effect Unit
